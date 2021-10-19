module Untyped where

import Data.Array (elem)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Show, ($), (*), (+), (-), (==))
import Result (Result(..))

data Expr
  = Num Number
  | Var String
  | Lam String Expr
  | App Expr Expr
  | Rec String Expr
  | Add
  | Sub
  | Mul
  | Eq

derive instance eqExpr :: Eq Expr
derive instance genericExpr :: Generic Expr _
instance showExpr :: Show Expr where
  show x = genericShow x

data Error
  = UndefinedVar String
  | NotAFunction Expr

derive instance eqError :: Eq Error
derive instance genericError :: Generic Error _
instance showError :: Show Error where
  show x = genericShow x

app2 :: Expr -> Expr -> Expr -> Expr
app2 f a b = App (App f a) b

add :: Expr -> Expr -> Expr
add = app2 Add

sub :: Expr -> Expr -> Expr
sub = app2 Sub

mul :: Expr -> Expr -> Expr
mul = app2 Mul

eq :: Expr -> Expr -> Expr
eq = app2 Eq

get :: String -> List (Tuple String Expr) -> Maybe Expr
get _ Nil = Nothing
get x (Cons (Tuple x' ex) _) | x == x' = Just ex
get x (Cons _ env) = get x env

eval :: Expr -> List (Tuple String Expr) -> Result Error Expr
eval expr env = case reduce expr env of
  Ok (App e1 e2) -> case Tuple (eval e1 env) (eval e2 env) of
    Tuple (Ok (Lam x e)) (Ok e2') -> eval (App (Lam x e) e2') env
    Tuple (Ok (Rec _ (Lam x e))) (Ok e2') -> eval (App (Lam x e) e2') env
    Tuple (Ok (App op (Num k1))) (Ok (Num k2)) | elem op [Add, Sub, Mul, Eq] -> reduce (App (App op (Num k1)) (Num k2)) env
    Tuple (Ok e1') (Ok e2') -> Ok (App e1' e2')
    Tuple (Err err) _ -> Err err
    Tuple _ (Err err) -> Err err
  Ok e -> Ok e
  Err err -> Err err

reduce :: Expr -> List (Tuple String Expr) -> Result Error Expr
reduce (Num k) _ = Ok (Num k)
reduce (Var x) env = case get x env of
  Just (Var x') | x == x' -> Ok (Var x)
  Just e -> reduce e (Cons (Tuple x (Rec x $ Var x)) env)
  Nothing -> Err (UndefinedVar x)
reduce (Lam x e) env = case reduce e (Cons (Tuple x (Var x)) env) of
  Ok (Rec y e') -> Ok (Rec y $ Lam x e')
  Ok e' -> Ok (Lam x e')
  Err err -> Err err
reduce (App e1 e2) env = case Tuple (reduce e1 env) (reduce e2 env) of
  Tuple (Ok (Num k)) _ -> Err (NotAFunction (Num k))
  Tuple (Ok (Lam x e)) (Ok e2') -> reduce e (Cons (Tuple x e2') env)
  Tuple (Ok (Rec x (Lam y e))) (Ok e2') -> Ok (App (Rec x $ Lam y e) e2')
  Tuple (Ok (Rec x e1')) (Ok (Rec x' e2')) | x == x' -> Ok (Rec x $ App e1' e2')
  Tuple (Ok (Rec x e1')) (Ok (Rec y e2')) -> Ok (Rec x $ Rec y $ App e1' e2')
  Tuple (Ok (Rec x e1')) (Ok e2') -> Ok (Rec x $ App e1' e2')
  Tuple (Ok e1') (Ok (Rec x e2')) -> Ok (Rec x $ App e1' e2')
  Tuple (Ok (App Add (Num k1))) (Ok (Num k2)) -> Ok (Num (k1 + k2))
  Tuple (Ok (App Sub (Num k1))) (Ok (Num k2)) -> Ok (Num (k1 - k2))
  Tuple (Ok (App Mul (Num k1))) (Ok (Num k2)) -> Ok (Num (k1 * k2))
  Tuple (Ok (App Eq (Num k1))) (Ok (Num k2)) | k1 == k2 -> Ok (Lam "True" $ Lam "False" $ Var "True")
  Tuple (Ok (App Eq (Num _))) (Ok (Num _)) -> Ok (Lam "True" $ Lam "False" $ Var "False")
  Tuple (Ok e1') (Ok e2') -> Ok (App e1' e2')
  Tuple (Err err) _ -> Err err
  Tuple _ (Err err) -> Err err
reduce (Rec x (Var x')) _ | x == x' = Ok (Rec x $ Var x)
reduce (Rec x e) env = reduce e (Cons (Tuple x (Rec x $ Var x)) env)
reduce e _ = Ok e
