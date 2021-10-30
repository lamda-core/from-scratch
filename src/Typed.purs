module Typed where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Dict (Dict, KV(..), empty, get, set)
import Result (Result(..))

data Expr
  = Any             -- _
  | Typ             -- Type
  | IntT            -- Int
  | Int Int         -- 42
  | Ctr String      -- C
  | Var String      -- x
  | As  Expr String -- e @ x
  | Ann Expr Expr   -- e : t
  | To  Expr Expr   -- p -> e
  | Or  Expr Expr   -- e1 | e2
  | And Expr Expr   -- (e1, e2)
  | App Expr Expr   -- e1 e2
  | Add             -- (+)
  | Sub             -- (-)
  | Mul             -- (*)

derive instance Eq Expr
derive instance Generic Expr _
instance Show Expr where
  -- show Any = "_"
  -- show Typ = "Type"
  -- show IntT = "Int"
  -- show (Int k) = show k
  -- show (Ctr c) = c
  -- show (Var x) = x
  -- show (As p x) = "(" <> show p <> ")@" <> x
  -- show (Ann e t) = "(" <> show e <> " : " <> show t <> ")"
  -- show (To p@(To _ _) e) = "(" <> show p <> ") -> " <> show e
  -- show (To p@(Or _ _) e) = "(" <> show p <> ") -> " <> show e
  -- show (To p@(And _ _) e@(And _ _)) = "(" <> show p <> ") -> (" <> show e <> ")"
  -- show (To p@(And _ _) e) = "(" <> show p <> ") -> " <> show e
  -- show (To p e@(And _ _)) = show p <> " -> (" <> show e <> ")"
  -- show (To p e) = show p <> " -> " <> show e
  -- show (Or e1@(And _ _) e2@(And _ _)) = "(" <> show e1 <> ") | (" <> show e2 <> ")"
  -- show (Or e1@(And _ _) e2) = "(" <> show e1 <> ") | " <> show e2
  -- show (Or e1 e2@(And _ _)) = show e1 <> " | (" <> show e2 <> ")"
  -- show (Or e1 e2) = show e1 <> " | " <> show e2
  -- show (And e1 e2) = show e1 <> ", " <> show e2
  -- show (App e1@(To _ _) e2@(To _ _)) = "(" <> show e1 <> ") (" <> show e2 <> ")"
  -- show (App e1@(To _ _) e2) = "(" <> show e1 <> ") " <> show e2
  -- show (App e1 e2@(To _ _)) = show e1 <> " (" <> show e2 <> ")"
  -- show (App e1@(Or _ _) e2@(Or _ _)) = "(" <> show e1 <> ") (" <> show e2 <> ")"
  -- show (App e1@(Or _ _) e2) = "(" <> show e1 <> ") " <> show e2
  -- show (App e1 e2@(Or _ _)) = show e1 <> " (" <> show e2 <> ")"
  -- show (App e1@(And _ _) e2@(And _ _)) = "(" <> show e1 <> ") (" <> show e2 <> ")"
  -- show (App e1@(And _ _) e2) = "(" <> show e1 <> ") " <> show e2
  -- show (App e1 e2@(And _ _)) = show e1 <> " (" <> show e2 <> ")"
  -- show (App (App Add e1) e2) = show e1 <> " + " <> show e2
  -- show (App (App Sub e1) e2) = show e1 <> " - " <> show e2
  -- show (App (App Mul e1) e2) = show e1 <> " * " <> show e2
  -- show (App e1 e2@(App _ _)) = show e1 <> " (" <> show e2 <> ")"
  -- show (App e1 e2) = show e1 <> " " <> show e2
  -- show Add = "(+)"
  -- show Sub = "(-)"
  -- show Mul = "(*)"
  show x = genericShow x

data Error
  = UndefinedName String
  | PatternMismatch Expr Expr
  | TypeMismatch Expr Expr

derive instance Eq Error
derive instance Generic Error _
instance Show Error where
  show x = genericShow x

type Env = Dict String Expr

app2 :: Expr -> Expr -> Expr -> Expr
app2 f e1 e2 = App (App f e1) e2

add2 :: Expr -> Expr -> Expr
add2 = app2 Add

sub2 :: Expr -> Expr -> Expr
sub2 = app2 Sub

mul2 :: Expr -> Expr -> Expr
mul2 = app2 Mul

occurs :: String -> Expr -> Boolean
occurs x (Var x') | x == x' = true
occurs x (As e y) | x /= y  = x `occurs` e
occurs x (Ann e t) = x `occurs` e || x `occurs` t
occurs x (To e1 e2) = not (x `occurs` e1) && x `occurs` e2
occurs x (Or e1 e2) = x `occurs` e1 || x `occurs` e2
occurs x (And e1 e2) = x `occurs` e1 || x `occurs` e2
occurs x (App e1 e2) = x `occurs` e1 || x `occurs` e2
occurs _ _ = false

unify :: Expr -> Expr -> Env -> Result Error (KV Expr Env)
unify Any b env = do
  KV b' _ <- eval b env
  Ok (b' `KV` env)
unify a Any env = do
  KV a' _ <- eval a env
  Ok (a' `KV` env)
unify (Var x) b env = case get x env of
  Just (Var x') | x == x' -> do
    KV b' _ <- eval b (set x b env)
    Ok (b' `KV` set x b' env)
  Just a -> unify a b env
  Nothing -> unify (Var x) b (set x (Var x) env)
unify a (Var x) env = case get x env of
  Just (Var x') | x == x' -> do
    KV a' _ <- eval a (set x a env)
    Ok (a' `KV` set x a' env)
  Just b -> unify a b env
  Nothing -> unify a (Var x) (set x (Var x) env)
unify (As a x) b env = unify a b (set x b env)
unify a (As b x) env = unify a b (set x a env)
unify (Ann a t) b env = do
  KV b' bt <- eval b env
  KV t' env' <- unify bt t env
  unify a (Ann b' t') env'
unify a (Ann b t) env = do
  KV a' at <- eval a env
  KV t' env' <- unify at t env
  unify b (Ann a' t') env'
unify (To a1 b1) (To a2 b2) env = do
  KV a env1 <- unify a1 a2 env
  KV b env2 <- unify b1 b2 env1
  Ok ((a `To` b) `KV` env2)
unify (Or a b) t env = case unify a t env of
  Ok result -> Ok result
  Err (TypeMismatch _ _) -> unify b t env
  Err err -> Err err
unify t (Or a b) env = case unify t a env of
  Ok result -> Ok result
  Err (TypeMismatch _ _) -> unify t b env
  Err err -> Err err
unify (And a1 b1) (And a2 b2) env = do
  KV a env1 <- unify a1 a2 env
  KV b env2 <- unify b1 b2 env1
  Ok ((a `And` b) `KV` env2)
unify (App a1 b1) (App a2 b2) env = do
  KV a env1 <- unify a1 a2 env
  KV b env2 <- unify b1 b2 env1
  Ok ((a `App` b) `KV` env2)
unify a b env | a == b = Ok (a `KV` env)
unify a b _ = Err (TypeMismatch a b)

eval :: Expr -> Env -> Result Error (KV Expr Expr)
eval Any _ = Ok (Any `KV` Any)
eval Typ _ = Ok (Typ `KV` Typ)
eval IntT _ = Ok (IntT `KV` Typ)
eval (Int k) _ = Ok (Int k `KV` IntT)
eval (Ctr x) env = eval (Var x) env
eval (Var x) env = case get x env of
  Just (Var x') | x == x' -> Ok (Var x `KV` Var x)
  Just (Ann (Ctr c) t) -> do
    KV t' _ <- eval t env
    Ok (Ctr c `KV` t')
  Just (Ann (Var x') t) | x == x' -> do
    KV t' _ <- eval t env
    Ok (Var x `KV` t')
  Just e -> do
    KV e' t <- eval e (set x (Var x) env)
    -- TODO: support recursion
    -- if x `occurs` e'
    --   then Ok ((e' `As` x) `KV` t)
    --   else Ok (e' `KV` t)
    Ok (e' `KV` t)
  Nothing -> Err (UndefinedName x)
eval (As e x) env = eval (Var x) (set x e env)
eval (Ann e t) env = do
  KV e' et <- eval e env
  KV t' _ <- unify et t env
  Ok (e' `KV` t')
eval (To p e) env = do
  KV _ env' <- unify p p env
  KV p' pt <- eval p env'
  KV e' et <- eval e env'
  Ok ((p' `To` e') `KV` (pt `To` et))
eval (Or e1 e2) env = do
  -- TODO: check for missing cases
  -- TODO: check for redundant cases
  KV e1' t1 <- eval e1 env
  KV e2' t2 <- eval e2 env
  KV t _ <- unify t1 t2 env
  Ok ((e1' `Or` e2') `KV` t)
eval (And e1 e2) env = do
  KV e1' t1 <- eval e1 env
  KV e2' t2 <- eval e2 env
  Ok ((e1' `And` e2') `KV` (t1 `And` t2))
eval (App (And p1 p2) e2) env = do
  KV p1' t1 <- eval (p1 `App` e2) env
  KV p2' t2 <- eval (p2 `App` e2) env
  Ok ((p1' `And` p2') `KV` (t1 `And` t2))
eval (App e1 e2) env = do
  KV e1' ab <- eval e1 env
  KV e2' a <- eval e2 env
  KV ab' _ <- unify ab (a `To` Any) env
  case ab' of
    To _ t -> do
      case KV e1' e2' of
        -- TODO: support recursion, but maybe can be encoded in Var instead of As
        -- | As  Expr String -- e @ x
        KV (To p e) _ -> case unify p e2 env of
          Ok (KV _ env') -> eval e env'
          Err (TypeMismatch p' e') -> Err (PatternMismatch p' e')
          Err err -> Err err
        KV (Or p1 p2) _ -> case eval (p1 `App` e2) env of
          Ok result -> Ok result
          Err (PatternMismatch _ _) -> eval (p2 `App` e2) env
          Err err -> Err err
        KV (App Add (Int k1)) (Int k2) -> eval (Int (k1 + k2)) empty
        KV (App Sub (Int k1)) (Int k2) -> eval (Int (k1 - k2)) empty
        KV (App Mul (Int k1)) (Int k2) -> eval (Int (k1 * k2)) empty
        _ -> Ok ((e1' `App` e2') `KV` t)
    _ -> Err (TypeMismatch ab (a `To` Any)) -- unreachable
eval Add _ = Ok (Add `KV` (Var "a" `To` (Var "a" `To` Var "a")))
eval Sub _ = Ok (Sub `KV` (Var "a" `To` (Var "a" `To` Var "a")))
eval Mul _ = Ok (Mul `KV` (Var "a" `To` (Var "a" `To` Var "a")))
