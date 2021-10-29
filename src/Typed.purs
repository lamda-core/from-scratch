module Typed where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Dict (Dict, KV(..), get, set)
import Result (Result(..))

data Expr
  = Any             -- _
  | Typ            -- Type
  | IntT            -- Int
  | Int Int         -- 42
  | Ctr String      -- C
  | Var String      -- x
  | To  Expr Expr   -- p -> e
  | Or  Expr Expr   -- e1 | e2
  | And Expr Expr   -- (e1, e2)
  | App Expr Expr   -- e1 e2
  | Ann Expr Expr   -- e : t
  | As  Expr String -- e @ x
  | Add             -- (+)
  | Sub             -- (-)
  | Mul             -- (*)

derive instance Eq Expr
derive instance Generic Expr _
instance Show Expr where
  show Any = "_"
  show Typ = "Type"
  show IntT = "Int"
  show (Int k) = show k
  show (Ctr c) = c
  show (Var x) = x
  show (As p x) = "(" <> show p <> ")@" <> x
  show (To p@(To _ _) e) = "(" <> show p <> ") -> " <> show e
  show (To p@(Or _ _) e) = "(" <> show p <> ") -> " <> show e
  show (To p@(And _ _) e@(And _ _)) = "(" <> show p <> ") -> (" <> show e <> ")"
  show (To p@(And _ _) e) = "(" <> show p <> ") -> " <> show e
  show (To p e@(And _ _)) = show p <> " -> (" <> show e <> ")"
  show (To p e) = show p <> " -> " <> show e
  show (Or e1@(And _ _) e2@(And _ _)) = "(" <> show e1 <> ") | (" <> show e2 <> ")"
  show (Or e1@(And _ _) e2) = "(" <> show e1 <> ") | " <> show e2
  show (Or e1 e2@(And _ _)) = show e1 <> " | (" <> show e2 <> ")"
  show (Or e1 e2) = show e1 <> " | " <> show e2
  show (And e1 e2) = show e1 <> ", " <> show e2
  show (App e1@(To _ _) e2@(To _ _)) = "(" <> show e1 <> ") (" <> show e2 <> ")"
  show (App e1@(To _ _) e2) = "(" <> show e1 <> ") " <> show e2
  show (App e1 e2@(To _ _)) = show e1 <> " (" <> show e2 <> ")"
  show (App e1@(Or _ _) e2@(Or _ _)) = "(" <> show e1 <> ") (" <> show e2 <> ")"
  show (App e1@(Or _ _) e2) = "(" <> show e1 <> ") " <> show e2
  show (App e1 e2@(Or _ _)) = show e1 <> " (" <> show e2 <> ")"
  show (App e1@(And _ _) e2@(And _ _)) = "(" <> show e1 <> ") (" <> show e2 <> ")"
  show (App e1@(And _ _) e2) = "(" <> show e1 <> ") " <> show e2
  show (App e1 e2@(And _ _)) = show e1 <> " (" <> show e2 <> ")"
  show (App (App Add e1) e2) = show e1 <> " + " <> show e2
  show (App (App Sub e1) e2) = show e1 <> " - " <> show e2
  show (App (App Mul e1) e2) = show e1 <> " * " <> show e2
  show (App e1 e2@(App _ _)) = show e1 <> " (" <> show e2 <> ")"
  show (App e1 e2) = show e1 <> " " <> show e2
  show (Ann e t) = "(" <> show e <> " : " <> show t <> ")"
  show Add = "(+)"
  show Sub = "(-)"
  show Mul = "(*)"

data Error
  = UndefinedName String
  | PatternMismatch Expr Expr
  | TypeMismatch Expr Expr
  | NotAFunction Expr

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

-- declare :: Expr -> Env -> Env
-- declare (Var x) env  = set x (Var x) env
-- declare (As e x) env = set x (Var x) $ declare e env
-- declare (To p1 p2) env = declare p2 $ declare p1 env
-- declare (And p1 p2) env = declare p2 $ declare p1 env
-- declare (App p1 p2) env = declare p2 $ declare p1 env
-- declare _ env = env

occurs :: String -> Expr -> Boolean
occurs x (Var x') | x == x' = true
occurs x (As e y) | x /= y  = x `occurs` e
occurs x (To e1 e2)  = not (x `occurs` e1) && x `occurs` e2
occurs x (Or e1 e2)  = x `occurs` e1 || x `occurs` e2
occurs x (And e1 e2) = x `occurs` e1 || x `occurs` e2
occurs x (App e1 e2) = x `occurs` e1 || x `occurs` e2
occurs _ _ = false

-- TODO: once a pattern variable is bound, replace any occurrances in the subsequent patterns
-- This might require to match over the entire case instead of only the pattern.
-- Example:
--  (x -> x -> y -> (x, y)) 1 1 2
--  (1 -> y -> (1, y)) 1 2
--  (y -> (1, y)) 2
--  (1, 2)
-- eval the subpattern in an env with x set
match = 0
-- match :: Expr -> Expr -> Env -> Result Error Env
-- match p e env = do
--   e' <- eval e env
--   case KV p e' of
--     KV Any _ -> Ok env
--     KV _ (Var _) -> Ok env
--     KV (Var x) _ -> Ok (set x e' env)
--     KV (To p1 p2) (To e1 e2) -> do
--       env1 <- match p1 e1 env
--       env2 <- match p2 e2 env1
--       Ok env2
--     KV (Or p1 p2) _ -> case match p1 e env of
--       Ok result -> Ok result
--       Err (PatternMismatch _ _) -> match p2 e env
--       Err err -> Err err
--     KV (And p1 p2) (And e1 e2) -> do
--       env1 <- match p1 e1 env
--       env2 <- match p2 e2 env1
--       Ok env2
--     KV (App p1 p2) (App e1 e2) -> do
--       env1 <- match p1 e1 env
--       env2 <- match p2 e2 env1
--       Ok env2
--     KV p' _ | p' == e -> Ok env
--     _ -> Err (PatternMismatch p e)

-- TODO: add tests
unify :: Expr -> Expr -> Env -> Result Error (KV Expr Env)
unify type1 type2 env = do
  KV t1 k1 <- eval type1 env
  KV t2 k2 <- eval type2 env
  case KV t1 t2 of
    KV (Var x) _ | k1 == Var x -> Ok (t2 `KV` set x t2 env)
    KV _ (Var x) | k2 == Var x -> Ok (t1 `KV` set x t1 env)
    _ -> Err (TypeMismatch t1 t2)

-- TODO: add tests
evalP :: Expr -> Env -> KV Expr Env
evalP (Var x) env = (Var x) `KV` (set x (Var x) env)
evalP (As p x) env = do
  let KV t env' = evalP p env
  t `KV` set x (Ann (Var x) t) env'
evalP (To p1 p2) env = do
  let KV t1 env1 = evalP p1 env
  let KV t2 env2 = evalP p2 env1
  (t1 `To` t2) `KV` env2
evalP (And p1 p2) env = do
  let KV t1 env1 = evalP p1 env
  let KV t2 env2 = evalP p2 env1
  (t1 `And` t2) `KV` env2
evalP (App p1 p2) env = do
  let KV t1 env1 = evalP p1 env
  let KV t2 env2 = evalP p2 env1
  (t1 `App` t2) `KV` env2
evalP p env = p `KV` env

-- TODO: make sure cases cover all possibilities
eval :: Expr -> Env -> Result Error (KV Expr Expr)
eval Any _ = Ok (Any `KV` Any)
eval Typ _ = Ok (Typ `KV` Typ)
eval IntT _ = Ok (IntT `KV` Typ)
eval (Int k) _ = Ok (Int k `KV` IntT)
eval (Ctr x) env = eval (Var x) env
eval (Ann (Ctr c) t) env = do
  KV t' _ <- eval t env
  Ok (Ctr c `KV` t')
eval (Var x) env = case get x env of
  Just (Var x') | x == x' -> Ok (Var x `KV` Var x)
  Just e -> do
    KV e' t <- eval e (set x (Var x) env)
    if x `occurs` e'
      then Ok ((e' `As` x) `KV` t)
      else Ok (e' `KV` t)
  Nothing -> Err (UndefinedName x)
eval (As e x) env = eval (Var x) (set x e env)
eval (To p e) env = do
  let KV pt env' = evalP p env
  KV e' et <- eval e env'
  Ok ((p `To` e') `KV` (pt `To` et))
eval (Or e1 e2) env = do
  KV e1' t1 <- eval e1 env
  KV e2' t2 <- eval e2 env
  KV t _ <- unify t1 t2 env
  Ok ((e1' `Or` e2') `KV` t)
eval (And e1 e2) env = do
  KV e1' t1 <- eval e1 env
  KV e2' t2 <- eval e2 env
  Ok ((e1' `And` e2') `KV` (t1 `And` t2))
-- eval (App expr1 expr2) env = do
--   e1 <- eval expr1 env
--   e2 <- eval expr2 env
--   case KV e1 e2 of
--     KV (Int k) _ -> Err (NotAFunction (Int k))
--     KV _ (Var _) -> Ok (e1 `App` e2)
--     KV _ (App _ _) -> Ok (e1 `App` e2)
--     KV (To p e) _ -> do
--       env' <- match p e2 env
--       eval e env'
--     KV (As p x) _ -> eval (p `App` e2) (set x p env)
--     KV (Or p1 p2) _ -> case eval (p1 `App` e2) env of
--       Ok (To Any e) -> Ok (Any `To` e)
--       Ok (To (Var x) e) -> Ok (Var x `To` e)
--       Ok (To p e) -> Ok ((p `To` e) `Or` (p2 `App` e2))
--       Ok e -> Ok e
--       Err (PatternMismatch _ _) -> eval (p2 `App` e2) env
--       Err err -> Err err
--     KV (And p1 p2) _ -> do
--       e1' <- eval (p1 `App` e2) env
--       e2' <- eval (p2 `App` e2) env
--       Ok (e1' `And` e2')
--     KV (App Add (Int k1)) (Int k2) -> Ok (Int (k1 + k2))
--     KV (App Sub (Int k1)) (Int k2) -> Ok (Int (k1 - k2))
--     KV (App Mul (Int k1)) (Int k2) -> Ok (Int (k1 * k2))
--     _ -> Ok (e1 `App` e2)
-- eval Add _ = Ok Add
-- eval Sub _ = Ok Sub
-- eval Mul _ = Ok Mul
eval e _ = Err (UndefinedName ("Not implemented: eval " <> show e))
