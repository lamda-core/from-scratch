module Untyped where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Dict (Dict, KV(..), get, set)
import Result (Result(..))

data Expr
  = Any             -- _
  | Int Int         -- 42
  | Ctr String      -- C
  | Var String      -- x
  | To  Expr Expr   -- p -> e
  | Or  Expr Expr   -- e1 | e2
  | And Expr Expr   -- (e1, e2)
  | App Expr Expr   -- e1 e2
  | As  Expr String -- e @ x
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

type Env = Dict String Expr

app2 :: Expr -> Expr -> Expr -> Expr
app2 f e1 e2 = App (App f e1) e2

add :: Expr -> Expr -> Expr
add = app2 Add

sub :: Expr -> Expr -> Expr
sub = app2 Sub

mul :: Expr -> Expr -> Expr
mul = app2 Mul

declare :: Expr -> Env -> Env
declare (Var x) env = set x (Var x) env
declare (To p1 p2) env = declare p2 $ declare p1 env
declare (And p1 p2) env = declare p2 $ declare p1 env
declare (App p1 p2) env = declare p2 $ declare p1 env
declare (As e x) env = set x (Var x) $ declare e env
declare _ env = env

occurs :: String -> Expr -> Boolean
occurs x (Var x') | x == x' = true
occurs x (To e1 e2)  = x `occurs` e1 || x `occurs` e2
occurs x (Or e1 e2)  = x `occurs` e1 || x `occurs` e2
occurs x (And e1 e2) = x `occurs` e1 || x `occurs` e2
occurs x (App e1 e2) = x `occurs` e1 || x `occurs` e2
occurs x (As e y) | x /= y = x `occurs` e
occurs _ _ = false

match :: Expr -> Expr -> Env -> Result Error Env
match p e env = do
  e' <- eval e env
  case KV p e' of
    KV Any _ -> Ok env
    KV _ (Var _) -> Ok env
    KV (Var x) _ -> case get x env of
      Just (Var x') | x == x' -> Ok (set x e' env)
      Just p' -> match p' e' env
      Nothing -> Ok (set x e' env)
    KV (To p1 p2) (To e1 e2) -> do
      env1 <- match p1 e1 env
      env2 <- match p2 e2 env1
      Ok env2
    KV (Or p1 p2) _ -> case match p1 e env of
      Ok result -> Ok result
      Err (PatternMismatch _ _) -> match p2 e env
      Err err -> Err err
    KV (And p1 p2) (And e1 e2) -> do
      env1 <- match p1 e1 env
      env2 <- match p2 e2 env1
      Ok env2
    KV (App p1 p2) (App e1 e2) -> do
      env1 <- match p1 e1 env
      env2 <- match p2 e2 env1
      Ok env2
    KV p' _ | p' == e -> Ok env
    _ -> Err (PatternMismatch p e)

eval :: Expr -> Env -> Result Error Expr
eval Any _ = Err (NotAValue Any)
eval (Int k) _ = Ok (Int k)
eval (Ctr c) _ = Ok (Ctr c)
eval (Var x) env = case get x env of
  Just (Var x') | x == x' -> Ok (Var x)
  Just e -> do
    e' <- eval e (set x (Var x) env)
    if x `occurs` e'
      then Ok (e' `As` x)
      else Ok e'
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
  case KV e1 e2 of
    KV (Int k) _ -> Err (NotAFunction (Int k))
    KV (To p e) _ -> do
      env' <- match p e2 env
      case KV p e2 of
        KV Any _ -> eval e env'
        KV (Var x) (Var y) -> eval e (set x (Var y) env')
        KV _ (Var y) -> Ok (e1 `App` Var y)
        _ -> eval e env'
    KV (Or p1 p2) _ -> case eval (p1 `App` e2) env of
      Ok e -> Ok e
      Err (PatternMismatch _ _) -> eval (p2 `App` e2) env
      Err err -> Err err
    KV (And p1 p2) _ -> do
      e1' <- eval (p1 `App` e2) env
      e2' <- eval (p2 `App` e2) env
      Ok (e1' `And` e2')
    KV (App Add (Int k1)) (Int k2) -> Ok (Int (k1 + k2))
    KV (App Sub (Int k1)) (Int k2) -> Ok (Int (k1 - k2))
    KV (App Mul (Int k1)) (Int k2) -> Ok (Int (k1 * k2))
    _ -> Ok (e1 `App` e2)
eval (As e x) env = eval (Var x) (set x e env)
eval Add _ = Ok Add
eval Sub _ = Ok Sub
eval Mul _ = Ok Mul
