module Pattern where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Result (Result(..))

data Expr
  = Any             -- _
  | Ctr String      -- C
  | Var String      -- x
  | To Expr Expr    -- x -> x
  | Or  Expr Expr   -- x | y
  | And Expr Expr   -- (x, y)
  | App Expr Expr   -- x y

derive instance Eq Expr
derive instance Generic Expr _
instance Show Expr where
  show x = genericShow x

data Error
  = UndefinedVar String
  | DoesNotMatch Expr Expr

derive instance Eq Error
derive instance Generic Error _
instance Show Error where
  show x = genericShow x

get :: forall a. String -> List (Tuple String a) -> Maybe a
get _ Nil = Nothing
get x (Tuple x' ex : _) | x == x' = Just ex
get x (_ : env) = get x env

set :: forall k v. Eq k => k -> v -> List (Tuple k v) -> List (Tuple k v)
set key value Nil = Tuple key value : Nil
set key value (Tuple key' _ : kvs) | key == key' = Tuple key value : kvs
set key value (Tuple key' value' : kvs) = Tuple key' value' : set key value kvs

match :: Expr -> Expr -> List (Tuple String Expr) -> Result Error (Tuple Expr (List (Tuple String Expr)))
match pattern expr env = do
  p <- eval pattern env
  e <- eval expr env
  case Tuple p e of
    Tuple Any _ -> Ok (e `Tuple` env)
    Tuple (Var x) (Var y) -> Ok (Var y `Tuple` (set x (Var y) env))
    Tuple _ (Var y) -> Ok (Var y `Tuple` env)
    Tuple (Ctr x) _ | e == Ctr x -> Ok (e `Tuple` env)
    Tuple (Var x) _ -> Ok (e `Tuple` set x e env)
    Tuple (To p1 p2) (To e1 e2) -> do
      Tuple e1' env1 <- match p1 e1 env
      Tuple e2' env2 <- match p2 e2 env1
      Ok ((e1' `To` e2') `Tuple` env2)
    Tuple (Or p1 p2) _ -> case match p1 e env of
      Ok result -> Ok result
      Err (DoesNotMatch _ _) -> match p2 e env
      Err err -> Err err
    Tuple (And p1 p2) (And e1 e2) -> do
      Tuple e1' env1 <- match p1 e1 env
      Tuple e2' env2 <- match p2 e2 env1
      Ok ((e1' `And` e2') `Tuple` env2)
    Tuple (App p1 p2) (App e1 e2) -> do
      Tuple e1' env1 <- match p1 e1 env
      Tuple e2' env2 <- match p2 e2 env1
      Ok ((e1' `App` e2') `Tuple` env2)
    _ -> Err (p `DoesNotMatch` e)

eval :: Expr -> List (Tuple String Expr) -> Result Error Expr
eval Any _ = Ok Any
eval (Ctr c) _ = Ok (Ctr c)
eval (Var x) env = case get x env of
  Just (Var x') | x == x' -> Ok (Var x)
  Just e -> eval e env
  Nothing -> Err (UndefinedVar x)
eval (To p e) env = do
  p' <- eval p env
  e' <- eval e env
  Ok (p' `To` e')
eval (Or e1 e2) env = do
  e1' <- eval e1 env
  e2' <- eval e2 env
  Ok (e1' `Or` e2')
eval (And e1 e2) env = do
  e1' <- eval e1 env
  e2' <- eval e2 env
  Ok (e1' `And` e2')
eval (App expr1 expr2) env = do
  e1 <- eval expr1 env
  e2 <- eval expr2 env
  case Tuple e1 e2 of
    Tuple (To Any e) _ -> eval e env
    Tuple (To (Var x) e) (Var y) -> eval e (x `Tuple` Var y : env)
    Tuple _ (Var y) -> Ok (e1 `App` Var y)
    Tuple (To p e) _ -> do
      Tuple _ env' <- match p e2 env
      eval e env'
    Tuple (Or p1 p2) _ -> case eval (p1 `App` e2) env of
      Ok e -> Ok e
      Err (DoesNotMatch _ _) -> eval (p2 `App` e2) env
      Err err -> Err err
    Tuple (And p1 p2) _ -> do
      e1' <- eval (p1 `App` e2) env
      e2' <- eval (p2 `App` e2) env
      Ok (e1' `And` e2')
    _ -> Ok (e1 `App` e2)
