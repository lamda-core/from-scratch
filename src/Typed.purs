module Typed where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Dict (Dict, KV(..), get, set, union)
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
  show Any = "_"
  show Typ = "Type"
  show IntT = "Int"
  show (Int k) = show k
  show (Ctr c) = c
  show (Var x) = x
  show (As p x) = "(" <> show p <> ")@" <> x
  show (Ann e t) = "(" <> show e <> " : " <> show t <> ")"
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
  show Add = "(+)"
  show Sub = "(-)"
  show Mul = "(*)"

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
occurs x (To e1 e2)  = not (x `occurs` e1) && x `occurs` e2
occurs x (Or e1 e2)  = x `occurs` e1 || x `occurs` e2
occurs x (And e1 e2) = x `occurs` e1 || x `occurs` e2
occurs x (App e1 e2) = x `occurs` e1 || x `occurs` e2
occurs _ _ = false

unify :: Expr -> Expr -> Env -> Result Error (KV Expr Env)
unify Any b env = Ok (KV b env)
unify a Any env = Ok (KV a env)
unify (Var x) b env = case get x env of
  Just (Var x') | x == x' -> Ok (b `KV` set x b env)
  Just a -> unify a b env
  Nothing -> Ok (b `KV` set x b env)
unify a (Var x) env = case get x env of
  Just (Var x') | x == x' -> Ok (a `KV` set x a env)
  Just b -> unify a b env
  Nothing -> Ok (a `KV` set x a env)
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
unify (Or a b) t env = case unify a t env of
  Ok result -> Ok result
  Err (TypeMismatch _ _) -> unify b t env
  Err err -> Err err
unify (To a1 b1) (To a2 b2) env = do
  KV a env1 <- unify a1 a2 env
  KV b1' _ <- eval b1 env1
  KV b env2 <- unify b1' b2 env
  Ok ((a `To` b) `KV` (env1 `union` env2))
unify t (Or a b) env = case unify t a env of
  Ok result -> Ok result
  Err (TypeMismatch _ _) -> unify t b env
  Err err -> Err err
unify (And a1 b1) (And a2 b2) env = do
  KV a env1 <- unify a1 a2 env
  KV b1' _ <- eval b1 env1
  KV b env2 <- unify b1' b2 env
  Ok ((a `And` b) `KV` (env1 `union` env2))
unify (App a1 b1) (App a2 b2) env =
  -- TODO!
  Err (UndefinedName ("Not implemented: unify (" <> show (App a1 b1) <> ") (" <> show (App a2 b2) <> ")"))
unify a b env | a == b = Ok (a `KV` env)
unify a b _ = Err (TypeMismatch a b)

-- TODO: make sure cases cover all possibilities
-- Consider adding MissingCases and RedundantCases errors
--    As eval happens, they can be updated until all cases are covered
-- TODO: try to replace `As` for recursive functions with a Var and always check if there's a recursive value or if it's a free variable
-- move out all the App cases to their own,
-- catch-all case should be explicit about cases that end recursion and simply re-`eval` other cases (?)
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
      then Ok ((e' `As` x) `KV` t)
      else Ok (e' `KV` t)
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
  KV e1' t1 <- eval e1 env
  KV e2' t2 <- eval e2 env
  KV t _ <- unify t1 t2 env
  Ok ((e1' `Or` e2') `KV` t)
eval (And e1 e2) env = do
  KV e1' t1 <- eval e1 env
  KV e2' t2 <- eval e2 env
  Ok ((e1' `And` e2') `KV` (t1 `And` t2))
eval (App e1 e2) env = do
  KV e1' ab <- eval e1 env
  KV e2' a <- eval e2 env
  KV ab' _ <- unify ab (a `To` Any) env
  case ab' of
    To a b -> case e1' of
      -- | As  Expr String -- e @ x
      -- | Ann Expr Expr   -- e : t
      To p e -> case unify p e2 env of
        Ok (KV _ env2) -> eval e env2
        Err (TypeMismatch p' e') -> Err (PatternMismatch p' e')
        Err err -> Err err
      Or p1 p2 -> case eval (p1 `App` e2) env of
        Ok result -> Ok result
        Err (PatternMismatch _ _) -> eval (p2 `App` e2) env
        Err err -> Err err
      -- | And Expr Expr   -- (e1, e2)
      -- | App Expr Expr   -- e1 e2
      -- | Add             -- (+)
      -- | Sub             -- (-)
      -- | Mul             -- (*)
      _ -> Ok ((e1' `App` e2') `KV` b)
    _ -> Err (UndefinedName ("TODO: impossible (?) error" <> show e1))

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
