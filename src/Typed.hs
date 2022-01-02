module Typed where

import Data.Bifunctor (second)
import Data.Char (chr, ord)
import Data.Map (Map, (!?))
import qualified Data.Map as Map

data Expr
  = Int Int
  | Var String
  | Lam Pattern Expr
  | App Expr Expr
  | Tup
  | Rec [(String, Expr)]
  | Ann Expr Typ
  | Or Expr Expr
  deriving (Eq, Show)

data Typ
  = TInt
  | TTyp [String]
  | TVar String
  | TFun Typ Typ
  | TApp Typ Typ
  | TTup
  | TRec [(String, Typ)]
  | TAnn String Typ
  | TOr Typ Typ
  deriving (Eq, Show)

data Pattern
  = PAny
  | PInt Int
  | PVar String
  | PCtr String [Pattern]
  | PTup [Pattern]
  -- TODO: PRec [(String, Pattern)]
  -- TODO: PAnn Pattern Typ
  -- TODO: POr Pattern Pattern
  -- TODO: PFun Pattern Pattern (?)
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
occurs x (TRec ((_, a) : kvs)) = occurs x a || occurs x (TRec kvs)
occurs x (TFun a b) = occurs x a || occurs x b
-- occurs x (Lam (PVar x') a) | x == x' = False
-- occurs x (Lam (PCtr _ ps) a) = any (\p -> occurs x (Lam p a)) ps
-- occurs x (Lam (PTup ps) a) = any (\p -> occurs x (Lam p a)) ps
-- occurs x (Lam _ a) = occurs x a
-- occurs x (App a b) = occurs x a || occurs x b
occurs x (TAnn x' t) = x == x' || x `occurs` t
occurs x (TOr a b) = x `occurs` a || x `occurs` b
occurs _ _ = False

bind :: String -> Typ -> Substitution
bind x t (TVar x') | x == x' = t
bind x t (TFun a b) = TFun (bind x t a) (bind x t b)
-- TODO: TTup
-- TODO: TCtr
bind x t (TRec kvs) = TRec (map (second (bind x t)) kvs)
-- bind x t (Lam p a) | x `occurs` Lam p a = Lam p (bind x t a)
-- bind x t (App a b) = App (bind x t a) (bind x t b)
-- bind x t (TAnn y b) = TAnn y (bind x t b)
bind x t (TOr a b) = TOr (bind x t a) (bind x t b)
bind _ _ t = t

unify :: Typ -> Typ -> Either Error Substitution
unify (TFun a1 b1) (TFun a2 b2) = unifyPair TFun a1 b1 a2 b2
-- TODO: TTup
-- TODO: TCtr
-- TODO: TRec
unify (TAnn x a) b = do
  s <- unify a b
  Right (s . bind x a)
unify a (TAnn x b) = unify (TAnn x a) b
-- TODO: TOr
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

-- def numberToBase(n, b):
--     if n == 0:
--         return [0]
--     digits = []
--     while n:
--         digits.append(int(n % b))
--         n //= b
--     return digits[::-1]

-- 0 -> [0]
-- 25 -> [25]
-- 26 -> [0, 0]
-- 51 -> [0, 25]
-- 52 -> [1, 0]

intToName :: Int -> String
intToName 0 = ""
intToName n = do
  let (q, r) = quotRem (n - 1) 26
  chr (r + ord 'a') : intToName q

newTypeName :: Int -> Typ -> String
newTypeName n t = do
  let x = intToName n
  if x `occurs` t then newTypeName (n + 1) t else x

-- declare :: Pattern -> Context -> Context
-- declare PAny ctx = ctx
-- declare (PInt _) ctx = ctx
-- declare (PVar x) ctx = Map.insert x (TVar x) ctx
-- declare (PTup ps) ctx = foldr declare ctx ps
-- declare (PCtr _ ps) ctx = foldr declare ctx ps

check :: Expr -> Context -> Either Error (Typ, Substitution)
check (Int _) _ = Right (TInt, id)
check (Var x) env = case env !? x of
  Just (TVar x') | x == x' -> Right (TVar x, id)
  Just t -> Right (t, id)
  Nothing -> Left (UndefinedName x)
check (Lam PAny a) ctx = do
  (t, s) <- check a ctx
  Right (TFun (TVar (newTypeName 1 t)) t, s)
check (Lam (PInt _) a) ctx = do
  (t, s) <- check a ctx
  Right (TFun TInt t, s)

-- check (Typ []) env = Right (Typ [], id)
-- check (Typ (alt : alts)) env = do
--   (t1, _) <- check (Var alt) env
--   (t2, _) <- check (Typ alts) env
--   Right (Typ (alt : alts), id)
-- check (Fun a b) env = do
--   T2 ta sa <- check a env
--   T2 tb sb <- check (sa b) env
--   Right (T2 (Fun (sb ta) tb) (sb <<< sa))
-- check Tup _ = Right (TTup, id)

-- check a _ = Left (UndefinedName $ "TODO: check " <> show a)

-- check (Ann a t) env = do
--   T2 ta sa <- check a env
--   -- TODO!
--   -- T2 t' s <- unify ta (sa t)
--   -- Right (T2 t' (s <<< sa))
--   Right (T2 t sa)

--- TODO: Think more about this: probably stay with `alternatives` and `spec => specialize`

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
