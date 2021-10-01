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
  | NotImplemented String Expr [(String, Expr)]
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
  Just e -> eval e (set x (Rec x (Var x)) env)
  Nothing -> Err (UndefinedVar x)
eval (Rec x (Var x')) _ | x == x' = Ok (Rec x (Var x))
eval (Rec x e) env = eval e (set x (Rec x (Var x)) env)
eval (Lam x e) env = case eval e (set x (Var x) env) of
  Ok (Rec y e') -> Ok (Rec y (Lam x e'))
  Ok e' -> Ok (Lam x e')
  Err err -> Err err
eval (App e1 e2) env = case (eval e1 env, eval e2 env) of
  (Ok (Num k), Ok _) -> Err (NotAFunction (Num k))
  (Ok (Var x), Ok e2') | e1 == Var x -> Ok (App (Var x) e2')
  (Ok (Rec x (Lam y e)), Ok e2') -> eval e (set x (Rec x (Var x)) $ set y e2' env)
  (Ok (Rec x e1'), Ok e2') -> Ok (Rec x (App e1' e2))
  (Ok (Lam x e), Ok e2') -> eval e (set x e2' env)
  -- TODO: save the recursive definition: Rec (String, Expr) Expr
  -- (Ok (App op e1'), Ok (Rec x e2')) -> Err (NotImplemented "+" (Rec x e2') env)
  (Ok (App Add (Num k1)), Ok (Num k2)) -> Ok (Num (k1 + k2))
  (Ok (App Sub (Num k1)), Ok (Num k2)) -> Ok (Num (k1 - k2))
  (Ok (App Mul (Num k1)), Ok (Num k2)) -> Ok (Num (k1 * k2))
  (Ok (App Eq (Num k1)), Ok (Num k2)) | k1 == k2 -> Ok (Lam "True" (Lam "False" (Var "True")))
  (Ok (App Eq (Num k1)), Ok (Num k2)) -> Ok (Lam "True" (Lam "False" (Var "False")))
  (Ok e1', Ok e2') -> Ok (App e1' e2')
  (Err err, _) -> Err err
  (_, Err err) -> Err err
eval e _ = Ok e
