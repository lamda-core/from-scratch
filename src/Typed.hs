module Typed where

import Data.Bifunctor (first, second)
import Data.Char (chr, ord)
import Data.Map (Map, (!?))
import qualified Data.Map as Map

data Expr
  = Int Int
  | Var String
  | Tup [Expr]
  | Rec [(String, Expr)]
  | Ann Expr Typ
  | Lam [(Pattern, Expr)] (Pattern, Expr)
  | App Expr Expr
  deriving (Eq, Show)

data Typ
  = TInt
  | TTyp [String]
  | TVar String
  | TTup [Typ]
  | TRec [(String, Typ)]
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
  deriving (Eq, Show)

type Context = Map String Typ

type Substitution = Typ -> Typ

data Error
  = UndefinedName String
  | NotAFunction Typ
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
occurs x (TFun a b) = occurs x a || occurs x b
occurs x (TApp a b) = occurs x a || occurs x b
occurs _ _ = False

bind :: String -> Typ -> Substitution
bind x t (TVar x') | x == x' = t
bind x t (TTup items) = TTup (map (bind x t) items)
bind x t (TRec fields) = TRec (map (second (bind x t)) fields)
bind x t (TFun a b) = TFun (bind x t a) (bind x t b)
bind x t (TApp a b) = TApp (bind x t a) (bind x t b)
bind _ _ t = t

unify :: Typ -> Typ -> Either Error Substitution
unify (TFun a1 b1) (TFun a2 b2) = unifyPair TFun a1 b1 a2 b2
-- TODO: TTup
-- TODO: TRec
-- TODO: TCtr
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
declare (PTup items) ctx = foldr declare ctx items
declare (PRec fields) ctx = foldr (declare . snd) ctx fields
declare (PCtr _ args) ctx = foldr declare ctx args

patternType :: Pattern -> Typ -> Context -> Typ
patternType PAny t _ = TVar (newTypeName 1 t)
patternType (PInt _) _ _ = TInt
patternType (PVar x) _ _ = TVar x
patternType (PTup items) t ctx = TTup (map (\p -> patternType p t ctx) items)
patternType (PRec fields) t ctx = TRec (map (\(x, p) -> (x, patternType p t ctx)) fields)
patternType (PCtr x args) t ctx = foldl TApp (TVar x) (map (\p -> patternType p t ctx) args)

typeOfPattern :: Pattern -> Int -> Context -> Either Error (Typ, Int)
typeOfPattern PAny i ctx = do
  let x = intToName i
  Right (TVar x, i + 1)
typeOfPattern (PInt _) i _ = Right (TInt, i)
typeOfPattern (PVar x) i _ = Right (TVar x, i)
typeOfPattern (PTup items) i ctx = do
  (ts, i) <- typeOfPatterns items i ctx
  Right (TTup ts, i)
typeOfPattern (PRec fields) i ctx = do
  (ts, i) <- typeOfPatterns (map snd fields) i ctx
  Right (TRec (zip (map fst fields) ts), i)
typeOfPattern (PCtr x args) i ctx = do
  (ctrType, _) <- typecheck (Var x) ctx
  (ts, i) <- typeOfPatterns args i ctx
  t <- foldType ctrType ts ctx
  Right (t, i)

foldType :: Typ -> [Typ] -> Context -> Either Error Typ
foldType (TFun a b) (c : ts) ctx = do
  s <- unify a c
  foldType b ts ctx
foldType t (_ : _) _ = Left (NotAFunction t)
foldType t [] _ = Right t

typeOfPatterns :: [Pattern] -> Int -> Context -> Either Error ([Typ], Int)
typeOfPatterns (p : ps) i ctx = do
  (t, i) <- typeOfPattern p i ctx
  (ts, i) <- typeOfPatterns ps i ctx
  Right (t : ts, i)
typeOfPatterns [] i _ = Right ([], i)

typecheck :: Expr -> Context -> Either Error (Typ, Substitution)
typecheck (Int _) _ = Right (TInt, id)
typecheck (Var x) env = case env !? x of
  Just t -> Right (t, id) -- TODO: rename type variables!
  Nothing -> Left (UndefinedName x)
typecheck (Tup items) ctx = do
  (ts, s) <- typecheckMany items ctx
  Right (TTup ts, s)
typecheck (Rec fields) ctx = do
  (ts, s) <- typecheckMany (map snd fields) ctx
  Right (TRec (zip (map fst fields) ts), s)
typecheck (Ann a t) ctx = do
  (ta, sa) <- typecheck a ctx
  s <- unify (sa t) ta
  Right (s ta, s . sa)
typecheck (Lam [] (p, a)) ctx = do
  (t, s) <- typecheck a (declare p ctx)
  Right (TFun (patternType p t ctx) t, s)
typecheck (Lam ((p, a) : cases) defaultCase) ctx = do
  (ta, sa) <- typecheck (Lam [] (p, a)) ctx
  (tb, sb) <- typecheck (Lam cases defaultCase) ctx
  s <- unify (sb ta) (sa tb)
  Right (s (sb ta), s . sb . sa)
typecheck (App a b) ctx = do
  (ta, sa) <- typecheck a ctx
  case ta of
    TFun ta1 ta2 -> do
      (_, s) <- typecheck (Ann b ta1) ctx
      Right (s ta2, s . sa)
    _ -> Left (NotAFunction ta)

typecheckMany :: [Expr] -> Context -> Either Error ([Typ], Substitution)
typecheckMany (a : items) ctx = do
  (ta, sa) <- typecheck a ctx
  (ts, ss) <- typecheckMany items ctx
  Right (ss ta : map sa ts, ss . sa)
typecheckMany [] _ = Right ([], id)

alternatives :: Typ -> Context -> Either Error [Pattern]
alternatives (TTyp (x : xs)) ctx = do
  (t, _) <- typecheck (Var x) ctx
  alts <- alternatives (TTyp xs) ctx
  Right (PCtr x (replicate (arity t) PAny) : alts)
alternatives (TTyp []) _ = Right []
alternatives (TVar x) ctx = do
  (kind, _) <- typecheck (Var x) ctx
  alternatives kind ctx
alternatives (TApp t _) ctx = alternatives t ctx
alternatives (TFun _ t) ctx = alternatives t ctx
alternatives _ _ = Right [PAny]

specialize :: [Pattern] -> Context -> Either Error [Pattern]
specialize (PCtr x args : ps) ctx = do
  (t, _) <- typecheck (Var x) ctx
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
