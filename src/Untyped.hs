module Untyped where

data Expr
  = Num Float
  | Var String
  | Let [(String, Expr)] Expr
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

lam :: [String] -> Expr -> Expr
lam xs e = foldr Lam e xs

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

eval :: Expr -> [(String, Expr)] -> Either Error Expr
eval (Num k) _ = Right (Num k)
eval (Var x) env = case get x env of
  Just (Var x') | x == x' -> Right (Var x)
  Just ex -> eval ex ((x, Var x) : env)
  Nothing -> Left (UndefinedVar x)
eval (Let vars e) env = eval e vars
eval (Lam x e) env = case eval e ((x, Var x) : env) of
  Right e' -> Right (Lam x e')
  Left err -> Left err
eval (App e1 e2) env = case eval e1 env of
  Right (Num k) -> Left (NotAFunction (Num k))
  Right (Var x) -> Right (App (Var x) e2)
  Right (Lam x e) -> eval e ((x, Let env e2) : env)
  Right (App Add e1') -> case (e1', eval e2 env) of
    (Num k1, Right (Num k2)) -> Right (Num (k1 + k2))
    (_, Right e2') -> Right (App (App Add e1') e2')
    (_, Left err) -> Left err
  Right (App Sub e1') -> case (e1', eval e2 env) of
    (Num k1, Right (Num k2)) -> Right (Num (k1 - k2))
    (_, Right e2') -> Right (App (App Sub e1') e2')
    (_, Left err) -> Left err
  Right (App Mul e1') -> case (e1', eval e2 env) of
    (Num k1, Right (Num k2)) -> Right (Num (k1 * k2))
    (_, Right e2') -> Right (App (App Mul e1') e2')
    (_, Left err) -> Left err
  Right (App Eq e1') -> case (e1', eval e2 env) of
    (Num k1, Right (Num k2)) | k1 == k2 -> Right (Lam "True" (Lam "False" (Var "True")))
    (Num k1, Right (Num k2)) -> Right (Lam "True" (Lam "False" (Var "False")))
    (_, Right e2') -> Right (App (App Eq e1') e2')
    (_, Left err) -> Left err
  Right (App a b) -> Right (App (App a b) e2)
  Right op -> case eval e2 env of
    Right e2' -> Right (App op e2')
    Left err -> Left err
  Left err -> Left err
eval op _ = Right op
