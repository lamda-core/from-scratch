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
  | Lam Expr        -- λp
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
occurs x (Ann e t) = x `occurs` e || x `occurs` t
occurs x (To e1 e2) = not (x `occurs` e1) && x `occurs` e2
occurs x (Or e1 e2) = x `occurs` e1 || x `occurs` e2
occurs x (And e1 e2) = x `occurs` e1 || x `occurs` e2
occurs x (App e1 e2) = x `occurs` e1 || x `occurs` e2
occurs _ _ = false

unify :: Expr -> Expr -> Env -> Result Error (KV Expr Env)
unify (Ann a t) b env = do
  KV a' t' <- eval (Ann a t) env
  KV b' _ <- eval (Ann b t') env
  unify a' b' env
unify a (Ann b t) env = do
  KV b' t' <- eval (Ann b t) env
  KV a' _ <- eval (Ann a t') env
  unify a' b' env
unify expr1 expr2 env = do
  KV a _ <- eval expr1 env
  KV b _ <- eval expr2 env
  case KV a b of
    KV Any _ -> Ok (b `KV` env)
    KV _ Any -> Ok (a `KV` env)
    KV (Var x) _ -> Ok (b `KV` set x b env)
    KV _ (Var x) -> Ok (a `KV` set x a env)
    KV (Lam (Var x)) _ -> unify (Var x) b (set x (Var x) env)
    KV _ (Lam (Var x)) -> unify a (Var x) (set x (Var x) env)
    KV (To a1 b1) (To a2 b2) -> do
      KV a' env1 <- unify a1 a2 env
      KV b' env2 <- unify b1 b2 env1
      Ok ((a' `To` b') `KV` env2)
    KV (Or e1 e2) _ -> case unify e1 b env of
      Ok result -> Ok result
      Err (TypeMismatch _ _) -> unify e2 b env
      Err err -> Err err
    KV _ (Or e1 e2) -> case unify a e1 env of
      Ok result -> Ok result
      Err (TypeMismatch _ _) -> unify a e2 env
      Err err -> Err err
    KV (And a1 b1) (And a2 b2) -> do
      KV a' env1 <- unify a1 a2 env
      KV b' env2 <- unify b1 b2 env1
      Ok ((a' `And` b') `KV` env2)
    KV (App a1 b1) (App a2 b2) -> do
      KV a' env1 <- unify a1 a2 env
      KV b' env2 <- unify b1 b2 env1
      Ok ((a' `App` b') `KV` env2)
    _ | a == b -> Ok (a `KV` env)
    _ -> Err (TypeMismatch a b)

-- TODO: add tests
unifyT :: Expr -> Expr -> Env -> Result Error (KV Expr Env)
unifyT t1 t2 env = do
  KV t env' <- unify (Lam t1) (Lam t2) env
  KV t' _ <- evalT t env'
  Ok (t' `KV` env')

-- TODO: add tests
evalT :: Expr -> Env -> Result Error (KV Expr Expr)
evalT e env = do
  KV t k <- eval (Lam e) env
  case t of
    Lam t' -> Ok (t' `KV` k)
    _ -> Ok (t `KV` k)

-- TODO: add tests
evalP :: Expr -> Env -> Result Error (KV Expr Env)
evalP (Lam (Var x)) env =
  Ok (Lam (Var x) `KV` (set x (Var x) env))
evalP (Lam (Ann p t)) env = do
  KV p' env1 <- evalP (Lam p) env
  KV _ env2 <- evalP (Lam t) env1
  Ok (p' `KV` env2)
evalP (Lam (To a b)) env = do
  KV a' env1 <- evalP (Lam a) env
  KV b' env2 <- evalP (Lam b) env1
  Ok ((a' `To` b') `KV` env2)
evalP (Lam (Or a b)) env = do
  KV a' env1 <- evalP (Lam a) env
  KV b' env2 <- evalP (Lam b) env1
  Ok ((a' `Or` b') `KV` env2)
evalP (Lam (And a b)) env = do
  KV a' env1 <- evalP (Lam a) env
  KV b' env2 <- evalP (Lam b) env1
  Ok ((a' `And` b') `KV` env2)
evalP (Lam (App a b)) env = do
  KV a' env1 <- evalP (Lam a) env
  KV b' env2 <- evalP (Lam b) env1
  Ok ((a' `App` b') `KV` env2)
evalP (Lam p) env = evalP p env
evalP p env = Ok (p `KV` env)

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
    if x `occurs` e'
      then Ok (Var x `KV` t)
      else Ok (e' `KV` t)
  Nothing -> Err (UndefinedName x)
eval (Lam e) env = do
  KV p env' <- evalP (Lam e) env
  KV _ t <- eval e env'
  Ok (p `KV` t)
eval (Ann e t) env = do
  KV e' te <- eval e env
  KV t' _ <- unifyT te t env
  Ok (e' `KV` t')
eval (To p e) env = do
  KV _ env' <- evalP p env
  KV p' tp <- eval p env'
  KV e' te <- eval e env'
  Ok ((p' `To` e') `KV` (tp `To` te))
eval (Or e1 e2) env = do
  -- TODO: check for missing cases
  -- TODO: check for redundant cases
  KV e1' t1 <- eval e1 env
  KV e2' t2 <- eval e2 env
  KV t _ <- unifyT t1 t2 env
  Ok ((e1' `Or` e2') `KV` t)
eval (And e1 e2) env = do
  KV e1' t1 <- eval e1 env
  KV e2' t2 <- eval e2 env
  Ok ((e1' `And` e2') `KV` (t1 `And` t2))
eval (App (And p1 p2) e2) env = do
  KV p1' t1 <- eval (p1 `App` e2) env
  KV p2' t2 <- eval (p2 `App` e2) env
  Ok ((p1' `And` p2') `KV` (t1 `And` t2))
eval (App (Or p1 p2) e2) env = case eval (p1 `App` e2) env of
  -- Ok (To Any e) -> Ok (Any `To` e)
  -- Ok (To (Var x) e) -> Ok (Var x `To` e)
  -- Ok (To p e) -> Ok ((p `To` e) `Or` (p2 `App` e2))
  Ok result -> Ok result
  Err (PatternMismatch _ _) -> eval (p2 `App` e2) env
  Err err -> Err err
eval (App e1 e2) env = do
  KV e1' ab <- eval e1 env
  KV e2' a <- eval e2 env
  KV ab' _ <- unifyT ab (a `To` Any) env
  case ab' of
    To _ t -> do
      case KV e1' e2' of
        KV _ (Var _) -> Ok ((e1' `App` e2') `KV` t) -- TODO: add test!
        KV _ (App _ _) -> Ok ((e1' `App` e2') `KV` t) -- TODO: add test!
        KV (Var x) _ -> case get x env of
          Just (Var _) -> Ok ((e1' `App` e2') `KV` t)
          Just p -> eval (p `App` e2') env
          Nothing -> Err (UndefinedName x) -- unreachable
        KV (To p e) _ -> do
          KV p' env1 <- evalP p env
          case unify p' e2' env1 of
            Ok (KV _ env2) -> eval e env2
            Err (TypeMismatch _ _) -> Err (PatternMismatch p e2)
            Err err -> Err err
        KV (Or p1 p2) _ -> eval ((p1 `Or` p2) `App` e2') env -- TODO: add test!
        KV (App Add (Int k1)) (Int k2) -> eval (Int (k1 + k2)) empty
        KV (App Sub (Int k1)) (Int k2) -> eval (Int (k1 - k2)) empty
        KV (App Mul (Int k1)) (Int k2) -> eval (Int (k1 * k2)) empty
        _ -> Ok ((e1' `App` e2') `KV` t)
    _ -> Err (TypeMismatch ab (a `To` Any)) -- unreachable
eval Add _ = Ok (Add `KV` (Var "a" `To` (Var "a" `To` Var "a")))
eval Sub _ = Ok (Sub `KV` (Var "a" `To` (Var "a" `To` Var "a")))
eval Mul _ = Ok (Mul `KV` (Var "a" `To` (Var "a" `To` Var "a")))
