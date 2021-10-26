module Untyped where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Result (Result(..))

data Expr
  = Any             -- _
  | Int Int         -- 42
  | Ctr String      -- C
  | Var String      -- x
  | To Expr Expr    -- p -> e
  | Or  Expr Expr   -- e1 | e2
  | And Expr Expr   -- (e1, e2)
  | App Expr Expr   -- e1 e2
  | Add             -- (+)
  | Sub             -- (-)
  | Mul             -- (*)

derive instance Eq Expr
derive instance Generic Expr _
instance Show Expr where
  show x = genericShow x

data Error
  = UndefinedVar String
  | PatternMismatch Expr Expr
  | NotAValue Expr
  | NotAFunction Expr

derive instance Eq Error
derive instance Generic Error _
instance Show Error where
  show x = genericShow x

app2 :: Expr -> Expr -> Expr -> Expr
app2 f e1 e2 = App (App f e1) e2

add :: Expr -> Expr -> Expr
add = app2 Add

sub :: Expr -> Expr -> Expr
sub = app2 Sub

mul :: Expr -> Expr -> Expr
mul = app2 Mul

get :: forall a. String -> List (Tuple String a) -> Maybe a
get _ Nil = Nothing
get x (Tuple x' ex : _) | x == x' = Just ex
get x (_ : env) = get x env

set :: forall k v. Eq k => k -> v -> List (Tuple k v) -> List (Tuple k v)
set key value Nil = Tuple key value : Nil
set key value (Tuple key' _ : kvs) | key == key' = Tuple key value : kvs
set key value (Tuple key' value' : kvs) = Tuple key' value' : set key value kvs

declare :: Expr -> List (Tuple String Expr) -> List (Tuple String Expr)
declare (Var x) env = set x (Var x) env
declare (To p1 p2) env = declare p2 $ declare p1 env
declare (And p1 p2) env = declare p2 $ declare p1 env
declare (App p1 p2) env = declare p2 $ declare p1 env
declare _ env = env

match :: Expr -> Expr -> List (Tuple String Expr) -> Result Error (List (Tuple String Expr))
match p e env = do
  e' <- eval e env
  case Tuple p e' of
    Tuple Any _ -> Ok env
    Tuple _ (Var _) -> Ok env
    Tuple (Var x) _ -> case get x env of
      Just (Var x') | x == x' -> Ok (set x e' env)
      Just p' -> match p' e' env
      Nothing -> Ok (x `Tuple` e' : env)
    Tuple (To p1 p2) (To e1 e2) -> do
      env1 <- match p1 e1 env
      env2 <- match p2 e2 env1
      Ok env2
    Tuple (Or p1 p2) _ -> case match p1 e env of
      Ok result -> Ok result
      Err (PatternMismatch _ _) -> match p2 e env
      Err err -> Err err
    Tuple (And p1 p2) (And e1 e2) -> do
      env1 <- match p1 e1 env
      env2 <- match p2 e2 env1
      Ok env2
    Tuple (App p1 p2) (App e1 e2) -> do
      env1 <- match p1 e1 env
      env2 <- match p2 e2 env1
      Ok env2
    Tuple p' _ | p' == e -> Ok env
    _ -> Err (PatternMismatch p e)

eval :: Expr -> List (Tuple String Expr) -> Result Error Expr
eval Any _ = Err (NotAValue Any)
eval (Int k) _ = Ok (Int k)
eval (Ctr c) _ = Ok (Ctr c)
eval (Var x) env = case get x env of
  Just (Var x') | x == x' -> Ok (Var x)
  Just e -> eval e env
  Nothing -> Err (UndefinedVar x)
eval (To (Or p1 p2) e) env = eval ((p1 `To` e) `Or` (p2 `To` e)) env
eval (To p e) env = do
  e' <- eval e (declare p env)
  Ok (p `To` e')
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
    Tuple (Int k) _ -> Err (NotAFunction (Int k))
    Tuple (To p e) _ -> do
      env' <- match p e2 env
      case Tuple p e2 of
        Tuple Any _ -> eval e env'
        Tuple (Var x) (Var y) -> eval e (x `Tuple` Var y : env')
        Tuple _ (Var y) -> Ok (e1 `App` Var y)
        _ -> eval e env'
    Tuple (Or p1 p2) _ -> case eval (p1 `App` e2) env of
      Ok e -> Ok e
      Err (PatternMismatch _ _) -> eval (p2 `App` e2) env
      Err err -> Err err
    Tuple (And p1 p2) _ -> do
      e1' <- eval (p1 `App` e2) env
      e2' <- eval (p2 `App` e2) env
      Ok (e1' `And` e2')
    Tuple (App Add (Int k1)) (Int k2) -> Ok (Int (k1 + k2))
    Tuple (App Sub (Int k1)) (Int k2) -> Ok (Int (k1 - k2))
    Tuple (App Mul (Int k1)) (Int k2) -> Ok (Int (k1 * k2))
    _ -> Ok (e1 `App` e2)
eval Add _ = Ok Add
eval Sub _ = Ok Sub
eval Mul _ = Ok Mul
