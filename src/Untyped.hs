module Untyped where

data Expr
  = Num Float
  | Var String
  | Lam String Expr
  | App Expr Expr
  | Rec String Expr
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

eval :: Expr -> [(String, Expr)] -> Result Error Expr
eval expr env = case reduce expr env of
  Ok (App e1 e2) -> case (eval e1 env, eval e2 env) of
    (Ok (Lam x e), Ok e2') -> eval (App (Lam x e) e2') env
    (Ok (Rec x (Lam y e)), Ok e2') -> eval (App (Lam y e) e2') env
    (Ok (App op (Num k1)), Ok (Num k2)) | op `elem` [Add, Sub, Mul, Eq] -> eval (App (App op (Num k1)) (Num k2)) env
    (Ok e1', Ok e2') -> Ok (App e1' e2')
    (Err err, _) -> Err err
    (_, Err err) -> Err err
  Ok e' -> Ok e'
  Err err -> Err err

reduce :: Expr -> [(String, Expr)] -> Result Error Expr
reduce (Num k) _ = Ok (Num k)
reduce (Var x) env = case get x env of
  Just (Var x') | x == x' -> Ok (Var x)
  Just e -> reduce e ((x, Rec x (Var x)) : env)
  Nothing -> Err (UndefinedVar x)
reduce (Lam x e) env = case reduce e ((x, Var x) : env) of
  Ok (Rec y e') -> Ok (Rec y (Lam x e'))
  Ok e' -> Ok (Lam x e')
  Err err -> Err err
reduce (App e1 e2) env = case (reduce e1 env, reduce e2 env) of
  (Ok (Num k), _) -> Err (NotAFunction (Num k))
  (Ok (Lam x e), Ok e2') -> reduce e ((x, e2') : env)
  (Ok (Rec x (Lam y e)), Ok e2') -> Ok (App (Rec x (Lam y e)) e2')
  (Ok (Rec x e1'), Ok (Rec x' e2')) | x == x' -> Ok (Rec x (App e1' e2'))
  (Ok (Rec x e1'), Ok e2') -> Ok (Rec x (App e1' e2'))
  (Ok e1', Ok (Rec x e2')) -> Ok (Rec x (App e1' e2'))
  (Ok (App Add (Num k1)), Ok (Num k2)) -> Ok (Num (k1 + k2))
  (Ok (App Sub (Num k1)), Ok (Num k2)) -> Ok (Num (k1 - k2))
  (Ok (App Mul (Num k1)), Ok (Num k2)) -> Ok (Num (k1 * k2))
  (Ok (App Eq (Num k1)), Ok (Num k2)) | k1 == k2 -> Ok (Lam "True" (Lam "False" (Var "True")))
  (Ok (App Eq (Num k1)), Ok (Num k2)) -> Ok (Lam "True" (Lam "False" (Var "False")))
  (Ok e1', Ok e2') -> Ok (App e1' e2')
  (Err err, _) -> Err err
  (_, Err err) -> Err err
reduce (Rec x (Var x')) _ | x == x' = Ok (Rec x (Var x))
reduce (Rec x e) env = reduce e ((x, Rec x (Var x)) : env)
reduce e _ = Ok e
