module Typed where

import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Prelude (bind, class Eq, class Show, show, ($), (<>), (==))
import Result (Result(..))

data Typ
  = NumT
  | VarT String
  | FunT Typ Typ
  | For String Typ

derive instance Eq Typ
derive instance Generic Typ _
instance Show Typ where
  show x = genericShow x

data Expr
  = Num Number
  | Var String
  | Lam String Expr
  | App Expr Expr
  | Rec String Expr
  | Ann Expr Typ
  | Add
  | Sub
  | Mul
  | Eq

derive instance Eq Expr
derive instance Generic Expr _
instance Show Expr where
  show x = genericShow x

data Error
  = UndefinedVar String
  | NotAFunction Expr Typ
  | TypeMismatch Typ Typ
  | MissingType String

derive instance Eq Error
derive instance Generic Error _
instance Show Error where
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

get :: forall a. String -> List (Tuple String a) -> Maybe a
get _ Nil = Nothing
get x (Tuple x' ex : _) | x == x' = Just ex
get x (_ : env) = get x env

set :: forall k v. Eq k => k -> v -> List (Tuple k v) -> List (Tuple k v)
set key value Nil = Tuple key value : Nil
set key value (Tuple key' _ : kvs) | key == key' = Tuple key value : kvs
set key value (Tuple key' value' : kvs) = Tuple key' value' : set key value kvs

-- eval :: Expr -> List (Tuple String Expr) -> Result Error Expr
-- eval expr env = case reduce expr env of
--   Ok (App e1 e2) -> case Tuple (eval e1 env) (eval e2 env) of
--     Tuple (Ok (Lam x e)) (Ok e2') -> eval (App (Lam x e) e2') env
--     Tuple (Ok (Rec _ (Lam x e))) (Ok e2') -> eval (App (Lam x e) e2') env
--     Tuple (Ok (App op (Num k1))) (Ok (Num k2)) | elem op [Add, Sub, Mul, Eq] -> reduce (App (App op (Num k1)) (Num k2)) env
--     Tuple (Ok e1') (Ok e2') -> Ok (App e1' e2')
--     Tuple (Err err) _ -> Err err
--     Tuple _ (Err err) -> Err err
--   Ok e -> Ok e
--   Err err -> Err err

reduceT :: Typ -> List (Tuple String Typ) -> Result Error Typ
reduceT NumT _ = Ok NumT
reduceT (VarT x) envT = case get x envT of
  Just (VarT x') | x == x' -> Ok (VarT x)
  Just t -> reduceT t (Tuple x (VarT x) : envT)
  Nothing -> Err (UndefinedVar x)
reduceT (FunT t1 t2) envT = case Tuple (reduceT t1 envT) (reduceT t2 envT) of
  Tuple (Ok (For x t1')) (Ok (For x' t2')) | x == x' -> Ok (For x $ FunT t1' t2')
  Tuple (Ok (For x t1')) (Ok (For y t2')) -> Ok (For x $ For y $ FunT t1' t2')
  Tuple (Ok (For x t1')) (Ok t2') -> Ok (For x $ FunT t1' t2')
  Tuple (Ok t1') (Ok (For x t2')) -> Ok (For x $ FunT t1' t2')
  Tuple (Ok t1') (Ok t2') -> Ok (FunT t1' t2')
  Tuple (Err err) _ -> Err err
  Tuple _ (Err err) -> Err err
reduceT (For x (VarT x')) _ | x == x' = Ok (For x $ VarT x)
reduceT (For x t) envT = reduceT t (Tuple x (For x $ VarT x) : envT)

-- TODO: simplify this!
unify :: Typ -> Typ -> List (Tuple String Typ) -> Result Error (Tuple Typ (List (Tuple String Typ)))
unify (FunT a1 a2) (FunT b1 b2) envT = do
  Tuple t1' envT1 <- unify a1 b1 envT
  Tuple t2' envT2 <- unify a2 b2 envT1
  t <- reduceT (FunT t1' t2') envT2
  Ok (Tuple t envT2)
unify t1 t2 envT = case Tuple (reduceT t1 envT) (reduceT t2 envT) of
  Tuple (Ok (For x (VarT _))) (Ok (For y (VarT _))) -> Ok (Tuple (For y (VarT y)) (set y (For y $ VarT y) $ set x (VarT y) envT))
  Tuple (Ok (For x (VarT _))) (Ok t2') -> Ok (Tuple t2' (set x t2' envT))
  Tuple (Ok t1') (Ok (For x (VarT _))) -> Ok (Tuple t1' (set x t1' envT))
  Tuple (Ok (For x t1')) (Ok (For y t2')) -> unify t1' t2' (set y (For y $ VarT y) $ set x (For x $ VarT x) envT)
  Tuple (Ok (For x t1')) (Ok t2') -> unify t1' t2' (set x (For x $ VarT x) envT)
  Tuple (Ok t1') (Ok (For x t2')) -> unify t1' t2' (set x (For x $ VarT x) envT)
  Tuple (Ok t1') (Ok t2') | t1' == t2' -> Ok (Tuple t1' envT)
  Tuple (Ok t1') (Ok t2') -> Err (TypeMismatch t1' t2')
  Tuple (Err err) _ -> Err err
  Tuple _ (Err err) -> Err err

-- check == reduce + unify

reduce :: Expr -> List (Tuple String Expr) -> List (Tuple String Typ) -> Result Error (Tuple Expr Typ)
reduce (Num k) _ _ =
  Ok (Tuple (Num k) NumT)

reduce (Var x) env envT = case get x env of
  Just (Var x') | x == x' -> Err (MissingType x)
  Just (Ann (Var x') t) | x == x' -> do
    t' <- reduceT t envT
    Ok (Tuple (Var x) t')
  Just e -> reduce e (Tuple x (Rec x $ Var x) : env) envT
  Nothing -> Err (UndefinedVar x)

reduce (Lam x e) env envT = let x' = x <> "'"
  in reduce (Ann (Lam x e) (For x $ For x' $ FunT (VarT x) (VarT x'))) env envT
reduce (Ann (Lam x e) (FunT t1 t2)) env envT = do
  Tuple e' et <- reduce e (Tuple x (Ann (Var x) t1) : env) envT
  Tuple t2' envT' <- unify t2 et envT
  t' <- reduceT (FunT t1 t2') envT'
  Ok (Tuple (Lam x e') t')

reduce (App (Ann e1 (FunT t1 t2)) e2) env envT = do
  let e1' = e1
  Tuple e2' t1' <- reduce e2 env envT
  Tuple _ envT' <- unify t1 t1' envT
  t2' <- reduceT t2 envT'
  case e1' of
    Lam x e -> reduce (Ann e t2') (Tuple x (Ann e2 t1) : env) envT'
    _ -> Ok (Tuple (App e1' e2') t2')
reduce (App (Ann e1 (For x t1)) e2) env envT =
  reduce (App (Ann e1 t1) e2) env (Tuple x (For x (VarT x)) : envT)
reduce (App (Ann e1 t) _) _ _ = Err (NotAFunction e1 t)
reduce (App e1 e2) env envT = do
  Tuple e1' t1 <- reduce e1 env envT
  reduce (App (Ann e1' t1) e2) env envT

-- reduce (App (Lam x e) ex) env envT = reduce e (Tuple x ex : env) envT
-- reduce (App (Ann (Lam x e) (FunT t1 t2)) ex) env envT = reduce (Ann e t2) (Tuple x (Ann ex t1) : env) envT
-- reduce (App e1 e2) env envT = case Tuple (reduce e1 env envT) (reduce e2 env envT) of
--   Tuple (Ok (Num k)) _ -> Err (NotAFunction (Num k))
--   Tuple (Ok (Lam x e)) (Ok e2') -> reduce e (Tuple x e2' : env)
--   Tuple (Ok (Rec x (Lam y e))) (Ok e2') -> Ok (App (Rec x $ Lam y e) e2')
--   Tuple (Ok (Rec x e1')) (Ok (Rec x' e2')) | x == x' -> Ok (Rec x $ App e1' e2')
--   Tuple (Ok (Rec x e1')) (Ok (Rec y e2')) -> Ok (Rec x $ Rec y $ App e1' e2')
--   Tuple (Ok (Rec x e1')) (Ok e2') -> Ok (Rec x $ App e1' e2')
--   Tuple (Ok e1') (Ok (Rec x e2')) -> Ok (Rec x $ App e1' e2')
--   Tuple (Ok (App Add (Num k1))) (Ok (Num k2)) -> Ok (Num (k1 + k2))
--   Tuple (Ok (App Sub (Num k1))) (Ok (Num k2)) -> Ok (Num (k1 - k2))
--   Tuple (Ok (App Mul (Num k1))) (Ok (Num k2)) -> Ok (Num (k1 * k2))
--   Tuple (Ok (App Eq (Num k1))) (Ok (Num k2)) | k1 == k2 -> Ok (Lam "True" $ Lam "False" $ Var "True")
--   Tuple (Ok (App Eq (Num _))) (Ok (Num _)) -> Ok (Lam "True" $ Lam "False" $ Var "False")
--   Tuple (Ok (Tuple e1' (For x t1))) (Ok (Tuple e2' _)) -> reduce (App (Ann e1' t1) e2') env (Tuple x (For x t1) : envT)
--   Tuple (Ok (Tuple e1' (FunT t1 t2))) (Ok (Tuple e2' t1')) -> do
--     Tuple _ envT' <- unify t1 t1' envT
--     t2' <- reduceT t2 envT'
--     Ok (Tuple (App e1' e2') t2')
--   Tuple (Ok (Tuple e1' t)) (Ok _) -> Err (NotAFunction e1' t)
--   Tuple (Err err) _ -> Err err
--   Tuple _ (Err err) -> Err err

reduce (Ann e (For x t)) env envT =
  reduce (Ann e t) env (Tuple x (For x $ VarT x) : envT)
reduce (Ann e t) env envT = do
  Tuple e' et <- reduce e env envT
  Tuple t' _ <- unify et t envT
  Ok (Tuple e' t')

-- reduce (Ann e (For x t)) env envT = reduce (Ann e t) env (Tuple x (For x $ VarT x) : envT)
-- reduce (Ann (Lam x e) (FunT t1 t2)) env envT = do
--   Tuple e' et <- reduce e (Tuple x (Ann (Var x) t1) : env) envT
--   Tuple t2' envT' <- unify t2 et envT
--   t' <- reduceT (FunT t1 t2') envT'
--   Ok (Tuple (Lam x e') t')
-- reduce (Ann e t) env envT = do
--   Tuple e' et <- reduce e env envT
--   Tuple t' _ <- unify et t envT
--   Ok (Tuple e' t')
-- reduce (Rec x (Var x')) _ | x == x' = Ok (Rec x $ Var x)
-- reduce (Rec x e) env = reduce e (Tuple x (Rec x $ Var x) : env)
-- reduce e _ = Ok e
reduce e _ _ = Err (UndefinedVar $ "Not implemented: " <> show e)

--  -- Subtype
--  check env other t2 =
--    let t1 = synth env other
--    in isSubtype t1 t2