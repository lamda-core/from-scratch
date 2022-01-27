module Typed where

import Data.Char (chr, ord)
import Data.Map (Map, member, (!?))
import qualified Data.Map as Map

data Expr
  = Any
  | Tup
  | IntT
  | Int Int
  | Var String
  | Typ [String]
  | For String Expr
  | Ann Expr Typ
  | Fun Typ Typ
  | App Expr Expr
  | Lam [(Pattern, Expr)]
  deriving (Eq, Show)

type Typ = Expr

type Pattern = Expr

type Substitution = Expr -> Expr

data Env = Env
  { seed :: Int,
    vars :: Map String Expr
  }
  deriving (Eq, Show)

data Error
  = UndefinedName String
  | CannotUnify Expr Expr
  deriving (Eq, Show)

empty :: Env
empty = Env {seed = 1, vars = Map.empty}

fromList :: [(String, Expr)] -> Env
fromList variables = empty {vars = Map.fromList variables}

has :: String -> Env -> Bool
has x env = x `member` vars env

get :: String -> Env -> Maybe Expr
get x env = vars env !? x

set :: String -> Expr -> Env -> Env
set x t env = env {vars = Map.insert x t (vars env)}

-- TODO: FIX THIS
defineType :: String -> [Typ] -> [(String, Typ)] -> Env -> Env
defineType name args ctrs env = do
  let kind = foldr Fun (Typ (map fst ctrs)) args
  let defineCtr env' (x, t) = set x t env'
  foldl defineCtr (set name kind env) ctrs

eval :: Expr -> Env -> Expr
eval (Var x) env = case get x env of
  Just (Ann (Var x') _) | x == x' -> Var x
  Just a | a /= Var x -> eval a env
  _ -> Var x
eval (For x a) env = For x (eval a (set x (Var x) env))
eval (Ann a _) env = eval a env
eval (Fun a b) env = Fun (eval a env) (eval b env)
eval (Lam alts) env = Lam (map (\(p, a) -> (eval p env, a)) alts)
eval (App a b) env = case eval a env of
  Lam alts -> match b alts env
  a' | a == a' -> App a (eval b env)
  a' -> eval (App a' b) env
eval a _ = a

match :: Expr -> [(Pattern, Expr)] -> Env -> Expr
match _ [] _ = Any
match a ((p, b) : alts) env = case unify p (eval a env) env of
  Right (_, env') -> eval b env'
  Left _ -> match a alts env

typecheck :: Expr -> Env -> Either Error (Typ, Env)
typecheck Any env = Right (Any, env)
typecheck Tup env = Right (Tup, env)
typecheck IntT env = Right (Typ [], env)
typecheck (Int _) env = Right (IntT, env)
typecheck (Var x) env = case get x env of
  Just (Var x') | x == x' -> Right (Var x, env)
  Just (Ann (Var x') t) | x == x' -> do
    (_, env') <- typecheck t env
    Right (instantiate t env')
  Just a -> typecheck a env
  Nothing -> Left (UndefinedName x)
typecheck (Typ _) env = Right (Typ [], env)
typecheck (For x a) env = typecheck a (set x (Var x) env)
typecheck (Ann a t) env = do
  (ta, env') <- typecheck a env
  unify t ta env'
typecheck (Fun a b) env = do
  (ta, env1) <- typecheck a env
  (tb, env2) <- typecheck b env1
  Right (Fun (eval ta env2) tb, env2)
typecheck (Lam []) env = Right (Fun Any Any, env)
typecheck (Lam ((p, a) : alts)) env = do
  (tp, env1) <- typecheck p env
  (ta, env2) <- typecheck a env1
  (ts, env3) <- typecheck (Lam alts) env2
  unify (Fun tp ta) ts env3
typecheck (App a b) env = do
  (ta, env1) <- typecheck a env
  (tb, env2) <- typecheck b env1
  let (x, env3) = newVar env2
  (_, env4) <- unify (eval ta env3) (Fun tb (Var x)) env3
  Right (eval (Var x) env4, env2)

unify :: Expr -> Expr -> Env -> Either Error (Expr, Env)
unify Any b env = Right (b, env)
unify a Any env = Right (a, env)
unify (For x a) b env = unify a b (set x (Var x) env)
unify a (For x b) env = unify a b (set x (Var x) env)
unify (Ann a ta) (Ann b tb) env = unify2 Ann (a, ta) (b, tb) env
unify (Fun a1 b1) (Fun a2 b2) env = unify2 Fun (a1, b1) (a2, b2) env
unify a a' env | a == a' = Right (a, env)
unify (Var x) b _ | x `occurs` b = Left (CannotUnify (Var x) b)
unify a (Var x) _ | x `occurs` a = Left (CannotUnify a (Var x))
unify (Var x) b env = Right (b, set x b env)
unify a (Var x) env = Right (a, set x a env)
unify a b _ = Left (CannotUnify a b)

unify2 :: (Expr -> Expr -> Expr) -> (Expr, Expr) -> (Expr, Expr) -> Env -> Either Error (Expr, Env)
unify2 f (a1, b1) (a2, b2) env = do
  (a, env1) <- unify a1 a2 env
  (b, env2) <- unify b1 b2 env1
  Right (f (eval a env2) b, env2)

occurs :: String -> Expr -> Bool
occurs x (Var x') = x == x'
occurs x (Ann a b) = occurs x a || occurs x b
occurs x (Fun a b) = occurs x a || occurs x b
occurs x (Lam ((p, a) : alts)) = (not (occurs x p) && occurs x a) || occurs x (Lam alts)
occurs x (App a b) = occurs x a || occurs x b
occurs _ _ = False

instantiate :: Expr -> Env -> (Expr, Env)
instantiate (For x a) env | has x env = do
  let (y, env') = newVar env
  rename x y a env'
instantiate (For x a) env = instantiate a (set x (Var x) env)
instantiate (Ann a t) env = instantiate2 Ann a t env
instantiate (Fun a b) env = instantiate2 Fun a b env
instantiate (App a b) env = instantiate2 App a b env
-- TODO: Lam -- does it make sense?
instantiate a env = (a, env)

instantiate2 :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Env -> (Expr, Env)
instantiate2 f a b env = do
  let (a', env1) = instantiate a env
  let (b', env2) = instantiate b env1
  (f a' b', env2)

rename :: String -> String -> Expr -> Env -> (Expr, Env)
rename x y (Var x') env | x == x' = (Var y, env)
rename x y (For y' a) env | y == y' = do
  let (z, env1) = newVar env
  let (a', env2) = rename y z a env1
  rename x y (For z a') env2
rename x y (For z a) env | x /= z = do
  let (a', env') = rename x y a env
  (For z a', env')
rename x y (Ann a t) env = rename2 x y Ann a t env
rename x y (Fun a b) env = rename2 x y Fun a b env
rename x y (App a b) env = rename2 x y App a b env
-- TODO: Lam -- does it make sense?
rename _ _ a env = (a, env)

rename2 :: String -> String -> (Expr -> Expr -> Expr) -> Expr -> Expr -> Env -> (Expr, Env)
rename2 x y f a b env = do
  let (a', env1) = rename x y a env
  let (b', env2) = rename x y b env1
  (f a' b', env2)

newVar :: Env -> (String, Env)
newVar env = do
  let x = intToName (seed env)
  let env' = env {seed = seed env + 1}
  if has x env
    then newVar env'
    else (x, set x (Var x) env')

intToName :: Int -> String
intToName 0 = ""
intToName i = do
  let (q, r) = quotRem (i - 1) 26
  chr (r + ord 'a') : intToName q

-- alternatives :: Typ -> Env -> Either Error [Pattern]
-- alternatives (Typ (x : xs)) env = do
--   (t, _) <- typecheck (Var x) env
--   alts <- alternatives (Typ xs) env
--   Right (PCtr x (replicate (arity t) PAny) : alts)
-- alternatives (Typ []) _ = Right []
-- alternatives (Var x) env = do
--   (kind, _) <- typecheck (Var x) env
--   alternatives kind env
-- alternatives (App t _) env = alternatives t env
-- alternatives (Fun _ t) env = alternatives t env
-- alternatives _ _ = Right [PAny]

-- specialize :: [Pattern] -> Env -> Either Error [Pattern]
-- specialize (PCtr x args : ps) env = do
--   (t, _) <- typecheck (Var x) env
--   alts <- alternatives t env
--   cases <- specialize ps env
--   Right (alts <> cases)
-- specialize [] _ = Right []
-- specialize _ _ = Right [PAny]

-- returnType :: Typ -> Typ
-- returnType (TFun _ t) = returnType t
-- returnType t = t

-- specialize :: Pattern -> Env -> Either Error [Pattern]
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

-- specializeCtr :: [String] -> String -> [Pattern] -> Env -> Either Error [Pattern]
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

-- specializeProduct :: [Pattern] -> Env -> Either Error [[Pattern]]
-- specializeProduct (p : qs) env = do
--   ps <- specialize p env
--   qss <- specializeProduct qs env
--   Right (concatMap (\p -> map (p :) qss) ps)
-- specializeProduct [] _ = Right [[]]

-- arity :: Typ -> Int
-- arity (Fun _ b) = 1 + arity b
-- arity _ = 0
