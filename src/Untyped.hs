module Untyped where

import System.Posix.Internals (c_dup2)

data Expr
  = Num Float
  | Var String
  | Rec String Expr
  | Lam String Expr
  | App Expr Expr
  | Add
  | Sub
  | Mul
  | Eq
  deriving (Show, Eq)

data Error
  = NotAFunction Expr
  | UndefinedVar String
  deriving (Show, Eq)

data Result err a
  = Ok a
  | Err err
  deriving (Show, Eq)

app :: Expr -> [Expr] -> Expr
app = foldl App

add :: Expr -> Expr -> Expr
add e = App (App Add e)

sub :: Expr -> Expr -> Expr
sub e = App (App Sub e)

mul :: Expr -> Expr -> Expr
mul e = App (App Mul e)

eq :: Expr -> Expr -> Expr
eq e = App (App Eq e)

get :: String -> [(String, Expr)] -> Maybe Expr
get x [] = Nothing
get x ((x', ex) : env) | x == x' = Just ex
get x (_ : env) = get x env

set :: String -> Expr -> [(String, Expr)] -> [(String, Expr)]
set x ex [] = [(x, ex)]
set x ex ((x', _) : env) | x == x' = (x, ex) : env
set x ex ((y, ey) : env) = (y, ey) : set x ex env

remove :: String -> [(String, Expr)] -> [(String, Expr)]
remove _ [] = []
remove x ((x', ex) : env) | x == x' = remove x env
remove x ((y, ey) : env) = (y, ey) : remove x env

merge :: [(String, Expr)] -> [(String, Expr)] -> [(String, Expr)]
merge [] env2 = env2
merge ((x, ex) : env1) env2 = case get x env2 of
  Just _ -> merge env1 env2
  Nothing -> (x, ex) : merge env1 env2

eval :: Expr -> [(String, Expr)] -> Result Error Expr
eval (Num k) _ = Ok (Num k)
eval (Var x) env = case get x env of
  Just (Var x') | x == x' -> Ok (Var x)
  Just (Lam y e) -> eval (Lam y e) (set x (Rec x (Var x)) env)
  Just e -> eval e (set x (Rec x e) env)
  Nothing -> Err (UndefinedVar x)
eval (Rec x (Var x')) _ | x == x' = Ok (Rec x (Var x))
eval (Rec x e) env = eval e env
eval (Lam x e) env = case eval e (set x (Var x) env) of
  Ok (Rec y e') -> Ok (Rec y (Lam x e'))
  Ok e' -> Ok (Lam x e')
  Err err -> Err err
eval (App (Num k) _) _ = Err (NotAFunction (Num k))
eval (App (Var x) e2) env = case get x env of
  Just (Var x') | x == x' -> case closure e2 env of
    -- Ok (Rec env' e2') -> Ok (Rec ((x, Var x) : env') (App (Var x) e2'))
    Ok e2' -> Ok (App (Var x) e2')
    Err err -> Err err
  Just e1' -> eval (App e1' e2) env
  Nothing -> Err (UndefinedVar x)
eval (App (Rec x e1) e2) env = case eval (Rec x e1) env of
  Ok (Rec y e1') -> Ok (Rec y (App e1' e2))
  Ok e1' -> eval (App e1' e2) env
  Err err -> Err err
eval (App (Lam x e) ex) env = case closure (Var x) (set x (Rec x ex) env) of
  Ok ex' -> eval e (set x ex' env)
  Err err -> Err err
eval (App (App op e1) e2) env | op `elem` [Add, Sub, Mul, Eq] = case (op, eval e1 env, eval e2 env) of
  (Add, Ok (Num k1), Ok (Num k2)) -> Ok (Num (k1 + k2))
  (Sub, Ok (Num k1), Ok (Num k2)) -> Ok (Num (k1 - k2))
  (Mul, Ok (Num k1), Ok (Num k2)) -> Ok (Num (k1 * k2))
  (Eq, Ok (Num k1), Ok (Num k2)) | k1 == k2 -> Ok (Lam "True" (Lam "False" (Var "True")))
  (Eq, Ok (Num k1), Ok (Num k2)) -> Ok (Lam "True" (Lam "False" (Var "False")))
  (_, Ok e1', Ok e2') -> Ok (App (App op e1') e2')
  (_, Err err, _) -> Err err
  (_, _, Err err) -> Err err
eval (App (App a b) e2) env = case eval (App a b) env of
  Ok e1' -> eval (App e1' e2) env
  Err err -> Err err
eval (App op e) env = case eval e env of
  Ok e' -> Ok (App op e')
  Err err -> Err err
eval Add _ = Ok Add
eval Sub _ = Ok Sub
eval Mul _ = Ok Mul
eval Eq _ = Ok Eq

-- eval e _ = Err (NotAFunction e)

closure :: Expr -> [(String, Expr)] -> Result Error Expr
closure e@(Num _) env = eval e env
closure e@(Var x) env = eval e env
closure e@(App _ _) env = eval e env
closure e _ = Err (NotAFunction e)