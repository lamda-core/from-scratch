module Pattern where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Result (Result(..))

data Expr
  = Any String      -- \x
  | Ctr String      -- C
  | Var String      -- x
  | Or  Expr Expr   -- x | y
  | And Expr Expr   -- (x, y)
  | Lam Expr Expr   -- x -> x
  | App Expr Expr   -- x y

derive instance Eq Expr
derive instance Generic Expr _
instance Show Expr where
  show x = genericShow x

data Error
  = UndefinedVar String
  | NotAFunction Expr
  | PatternMismatch Expr Expr
  -- TODO: REMOVE THIS
  | TODO Expr (List (Tuple String Expr))

derive instance Eq Error
derive instance Generic Error _
instance Show Error where
  show x = genericShow x

get :: forall a. String -> List (Tuple String a) -> Maybe a
get _ Nil = Nothing
get x (Tuple x' ex : _) | x == x' = Just ex
get x (_ : env) = get x env

eval :: Expr -> List (Tuple String Expr) -> Result Error Expr
eval (Any x) _ = Ok (Any x)
eval (Ctr x) _ = Ok (Ctr x)
eval (Var x) env = case get x env of
  Just (Var x') | x == x' -> Ok (Var x)
  Just e -> eval e env
  Nothing -> Err (UndefinedVar x)
eval (Or e1 e2) env = do
  e1' <- eval e1 env
  e2' <- eval e2 env
  Ok (Or e1' e2')
eval (And e1 e2) env = do
  e1' <- eval e1 env
  e2' <- eval e2 env
  Ok (And e1' e2')
eval (Lam (Any x) e) env = do
  e' <- eval e (Tuple x (Var x) : env)
  Ok (Lam (Any x) e')
eval (Lam p e) env = do
  -- TODO: test And/Or patterns with Any
  p' <- eval p env
  e' <- eval e env
  Ok (Lam p' e')
eval (App e1 e2) env = do
  e1' <- eval e1 env
  e2' <- eval e2 env
  case e1' of
    Any x -> Err (NotAFunction (Any x))
    Ctr x -> Ok (App (Ctr x) e2')
    Var x -> Ok (App (Var x) e2')
    Lam (Any x) e -> eval e (Tuple x e2' : env)
    _ -> Err (NotAFunction e1')
