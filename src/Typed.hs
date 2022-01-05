module Typed where

import Data.Bifunctor (second)
import Data.Char (chr, ord)
import Data.Map (Map, (!?))
import qualified Data.Map as Map

data Expr
  = Int Int
  | Var String
  | Tup [Expr]
  | Rec [(String, Expr)]
  | Ann Expr Typ
  | Lam Pattern Expr
  | Or Expr Expr
  | App Expr Expr
  deriving (Eq, Show)

data Typ
  = TInt
  | TTyp [String]
  | TVar String
  | TTup [Typ]
  | TRec [(String, Typ)]
  | TAnn String Typ
  | TFun Typ Typ
  | TApp Typ Typ
  deriving (Eq, Show)

data Pattern
  = PAny
  | PInt Int
  | PVar String
  | PCtr String [Pattern]
  | PTup [Pattern]
  | PRec [(String, Pattern)]
  | PAnn Pattern Typ
  deriving (Eq, Show)

type Context = Map String Typ

type Substitution = Typ -> Typ

data Error
  = UndefinedName String
  | NotAFunction Expr Typ
  | TypeMismatch Typ Typ
  | NumArgsMismatch {ctr :: String, expected :: Int, got :: Int}
  | RedundantPattern Pattern
  | MissingPatterns [Pattern]
  deriving (Eq, Show)

arity :: Typ -> Int
arity (TFun _ b) = 1 + arity b
arity _ = 0

defineType :: String -> [Typ] -> [(String, Typ)] -> Context -> Context
defineType name args ctrs ctx = do
  let kind = foldr TFun (TTyp (map fst ctrs)) args
  let defineCtr ctx' (x, t) = Map.insert x t ctx'
  foldl defineCtr (Map.insert name kind ctx) ctrs

occurs :: String -> Typ -> Bool
occurs x (TVar x') = x == x'
occurs x (TTup items) = any (occurs x) items
occurs x (TRec fields) = any (occurs x . snd) fields
occurs x (TAnn x' t) = x == x' || x `occurs` t
occurs x (TFun a b) = occurs x a || occurs x b
occurs x (TApp a b) = occurs x a || occurs x b
occurs _ _ = False

bind :: String -> Typ -> Substitution
bind x t (TVar x') | x == x' = t
bind x t (TTup items) = TTup (map (bind x t) items)
bind x t (TRec fields) = TRec (map (second (bind x t)) fields)
bind x t (TAnn x' _) | x == x' = t
bind x t (TFun a b) = TFun (bind x t a) (bind x t b)
bind x t (TApp a b) = TApp (bind x t a) (bind x t b)
bind _ _ t = t

unify :: Typ -> Typ -> Either Error Substitution
unify (TFun a1 b1) (TFun a2 b2) = unifyPair TFun a1 b1 a2 b2
-- TODO: TCtr
-- TODO: TRec
unify (TAnn x a) b = do
  s <- unify a b
  Right (s . bind x a)
unify a (TAnn x b) = unify (TAnn x a) b
unify a a' | a == a' = Right id
unify (TVar x) b | x `occurs` b = Left (TypeMismatch (TVar x) b)
unify a (TVar x) | x `occurs` a = Left (TypeMismatch a (TVar x))
unify (TVar x) b = Right (bind x b)
unify a (TVar x) = Right (bind x a)
unify a b = Left (TypeMismatch a b)

unifyPair :: (Typ -> Typ -> Typ) -> Typ -> Typ -> Typ -> Typ -> Either Error Substitution
unifyPair f a1 b1 a2 b2 = do
  sa <- unify a1 a2
  sb <- unify (sa b1) (sa b2)
  Right (sb . sa)

intToName :: Int -> String
intToName 0 = ""
intToName n = do
  let (q, r) = quotRem (n - 1) 26
  chr (r + ord 'a') : intToName q

newTypeName :: Int -> Typ -> String
newTypeName n t = do
  let x = intToName n
  if x `occurs` t then newTypeName (n + 1) t else x

declare :: Pattern -> Context -> Context
declare PAny ctx = ctx
declare (PInt _) ctx = ctx
declare (PVar x) ctx = Map.insert x (TVar x) ctx
declare (PTup ps) ctx = foldr declare ctx ps
-- TODO: PRec
declare (PCtr _ ps) ctx = foldr declare ctx ps

-- TODO: PAnn
patternType :: Typ -> Pattern -> Typ
patternType t PAny = TVar (newTypeName 1 t)
patternType _ (PInt _) = TInt
patternType _ (PVar x) = TVar x
patternType t (PTup ps) = TTup (map (patternType t) ps)
-- TODO: PRec
patternType t (PCtr x ps) = foldl TApp (TVar x) (map (patternType t) ps)

-- TODO: PAnn

check :: Expr -> Context -> Either Error (Typ, Substitution)
check (Int _) _ = Right (TInt, id)
check (Var x) env = case env !? x of
  Just (TVar x') | x == x' -> Right (TVar x, id)
  Just t -> Right (t, id)
  Nothing -> Left (UndefinedName x)
check (Tup items) ctx = do
  let checkItems :: [Expr] -> Either Error ([Typ], Substitution)
      checkItems (a : items) = do
        (ta, sa) <- check a ctx
        (ts, ss) <- checkItems items
        Right (ss ta : map sa ts, ss . sa)
      checkItems [] = Right ([], id)
  (ts, s) <- checkItems items
  Right (TTup ts, s)
check (Rec fields) ctx = do
  let checkFields :: [(String, Expr)] -> Either Error ([(String, Typ)], Substitution)
      checkFields ((k, a) : fields) = do
        (ta, sa) <- check a ctx
        (ts, ss) <- checkFields fields
        Right ((k, ss ta) : map (second sa) ts, ss . sa)
      checkFields [] = Right ([], id)
  (ts, s) <- checkFields fields
  Right (TRec ts, s)
check (Ann a t) ctx = do
  (ta, sa) <- check a ctx
  s <- unify (sa t) ta
  Right (s ta, s . sa)
check (Lam p a) ctx = do
  (t, s) <- check a (declare p ctx)
  Right (TFun (patternType t p) t, s)
check (Or a b) ctx = do
  (ta, sa) <- check a ctx
  (t, s) <- check (Ann b ta) ctx
  Right (t, s . sa)
check (App a b) ctx = do
  (ta, sa) <- check a ctx
  case ta of
    TFun ta1 ta2 -> do
      (_, s) <- check (Ann b ta1) ctx
      -- TODO: check exhaustive patterns here!
      Right (s ta2, s . sa)
    _ -> Left (NotAFunction a ta)

alternatives :: Typ -> Context -> Either Error [Pattern]
alternatives (TTyp (x : xs)) ctx = do
  (t, _) <- check (Var x) ctx
  alts <- alternatives (TTyp xs) ctx
  Right (PCtr x (replicate (arity t) PAny) : alts)
alternatives (TTyp []) _ = Right []
alternatives (TVar x) ctx = do
  (kind, _) <- check (Var x) ctx
  alternatives kind ctx
alternatives (TApp t _) ctx = alternatives t ctx
alternatives (TFun _ t) ctx = alternatives t ctx
alternatives _ _ = Right [PAny]

specialize :: [Pattern] -> Context -> Either Error [Pattern]
specialize (PCtr x args : ps) ctx = do
  (t, _) <- check (Var x) ctx
  alts <- alternatives t ctx
  cases <- specialize ps ctx
  Right (alts <> cases)
specialize [] _ = Right []
specialize _ _ = Right [PAny]

-- checkT :: Typ -> Context -> Either Error (Typ, Substitution)
-- checkT (TVar x) env = check (Var x) env

-- returnType :: Typ -> Typ
-- returnType (TFun _ t) = returnType t
-- returnType t = t

-- specialize :: Pattern -> Context -> Either Error [Pattern]
-- specialize (PCtr x ps) env = do
--   (typ, _) <- check (Var x) env
--   (kind, _) <- checkT (returnType typ) env
--   case kind of
--     TTyp alts -> specializeCtr alts x ps env
--     _ -> Right [PAny]
-- specialize (PTup ps) env = do
--   cases <- specializeProduct ps env
--   Right (map PTup cases)
-- specialize _ _ = Right [PAny]

-- specializeCtr :: [String] -> String -> [Pattern] -> Context -> Either Error [Pattern]
-- specializeCtr (x : alts) x' ps env | x == x' = do
--   (t, _) <- check (Var x) env
--   if length ps == arity t
--     then do
--       args <- specializeProduct ps env
--       cases <- specializeCtr alts x ps env
--       Right (map (PCtr x) args <> cases)
--     else Left (NumArgsMismatch {ctr = x, expected = arity t, got = length ps})
-- specializeCtr (x : alts) y ps env = do
--   (t, _) <- check (Var x) env
--   cases <- specializeCtr alts y ps env
--   Right (PCtr x (replicate (arity t) PAny) : cases)
-- specializeCtr [] _ _ _ = Right []

-- specializeProduct :: [Pattern] -> Context -> Either Error [[Pattern]]
-- specializeProduct (p : qs) env = do
--   ps <- specialize p env
--   qss <- specializeProduct qs env
--   Right (concatMap (\p -> map (p :) qss) ps)
-- specializeProduct [] _ = Right [[]]
