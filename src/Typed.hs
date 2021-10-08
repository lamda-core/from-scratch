module Typed where

data Expr
  = Num !Float
  | Var !String
  | Lam !String !Expr
  | App !Expr !Expr
  | Rec !String !Expr
  | Ann !Expr !Type
  | Add
  | Sub
  | Mul
  | Eq
  deriving (Show, Eq)

data Type
  = TypT
  | NumT
  | AnyT !String
  | VarT !String
  | FunT !Type !Type
  deriving (Show, Eq)

data Error
  = NotAFunction Expr Type
  | TypeMismatch Type Type
  | UndefinedVar String
  deriving (Show, Eq)

data Result err a
  = Ok a
  | Err err
  deriving (Show, Eq)

app :: Expr -> [Expr] -> Expr
app = foldl App

add :: Expr -> Expr -> Expr
add e = App (App Add e)

sub :: Expr -> Expr -> Expr
sub e = App (App Sub e)

mul :: Expr -> Expr -> Expr
mul e = App (App Mul e)

eq :: Expr -> Expr -> Expr
eq e = App (App Eq e)

get :: Eq k => k -> [(k, v)] -> Maybe v
get key [] = Nothing
get key ((key', value) : env) | key == key' = Just value
get key (_ : env) = get key env

-- eval :: Expr -> [(String, (Expr, Type))] -> Result Error (Expr, Type)
-- eval expr env = case reduce expr env of
--   -- Ok (App e1 e2) -> case (eval e1 env, eval e2 env) of
--   --   (Ok (Lam x e), Ok e2') -> eval (App (Lam x e) e2') env
--   --   (Ok (Rec x (Lam y e)), Ok e2') -> eval (App (Lam y e) e2') env
--   --   (Ok (App op (Num k1)), Ok (Num k2)) | op `elem` [Add, Sub, Mul, Eq] -> eval (App (App op (Num k1)) (Num k2)) env
--   --   (Ok e1', Ok e2') -> Ok (App e1' e2')
--   --   (Err err, _) -> Err err
--   --   (_, Err err) -> Err err
--   Ok e' -> Ok e'
--   Err err -> Err err

unify :: Type -> Type -> [(String, Type)] -> Result Error (Type, [(String, Type)])
unify (VarT x) t2 envT = case reduceT (VarT x) envT of
  Ok (t1, _) -> unify t1 t2 envT
  Err err -> Err err
unify t1 (VarT x) envT = case reduceT (VarT x) envT of
  Ok (t2, _) -> unify t1 t2 envT
  Err err -> Err err
unify (AnyT x) t envT = Ok (t, (x, t) : envT)
unify t (AnyT x) envT = Ok (t, (x, t) : envT)
unify (FunT a1 a2) (FunT b1 b2) envT = case unify a1 b1 envT of
  Ok (t1, envT1) -> case unify a2 b2 envT1 of
    Ok (t2, envT2) -> Ok (FunT t1 t2, envT2)
    Err err -> Err err
  Err err -> Err err
unify t1 t2 envT | t1 == t2 = Ok (t1, envT)
unify t1 t2 _ = Err (TypeMismatch t1 t2)

reduceT :: Type -> [(String, Type)] -> Result Error (Type, Type)
reduceT TypT _ = Ok (TypT, TypT)
reduceT NumT _ = Ok (NumT, TypT)
reduceT (AnyT x) _ = Ok (AnyT x, TypT)
reduceT (VarT x) envT = case get x envT of
  Just (VarT x') | x == x' -> Ok (VarT x, VarT x)
  Just t -> reduceT t ((x, VarT x) : envT)
  Nothing -> Err (UndefinedVar x)
reduceT (FunT t1 t2) env = case (reduceT t1 env, reduceT t2 env) of
  (Ok (t1', k1), Ok (t2', k2)) -> Ok (FunT t1' t2', FunT k1 k2)
  (Err err, _) -> Err err
  (_, Err err) -> Err err

reduce :: Expr -> [(String, Expr)] -> [(String, Type)] -> Result (Error, [Expr]) (Expr, Type)
reduce (Num k) _ _ = Ok (Num k, NumT)
reduce (Var x) env envT = case get x env of
  Just (Var x') | x == x' -> Ok (Var x, AnyT x)
  Just (Ann (Var x') t) | x == x' -> Ok (Var x, t)
  Just e -> reduce e ((x, Rec x (Var x)) : env) envT
  Nothing -> Err (UndefinedVar x, [Var x])
reduce (Lam x e) env envT = case reduce e ((x, Ann (Var x) (VarT x)) : env) envT of
  Ok (Rec y e', t2) -> Ok (Rec y (Lam x e'), FunT (AnyT x) t2)
  Ok (e', t2) -> Ok (Lam x e', FunT (AnyT x) t2)
  Err (err, stack) -> Err (err, Lam x e : stack)
reduce (App e1 e2) env envT = case (reduce e1 env envT, reduce e2 env envT) of
  (Ok (Lam x e, FunT t1 t2), Ok (e2', e2t)) -> case unify t1 e2t envT of
    -- TODO: unify (e : t) with t2 (?)
    Ok (t1', envT1) -> reduce e ((x, Ann e2' t1') : env) envT1
    -- Ok (t1', envT1) -> case reduce e ((x, Ann e2' t1') : env) envT1 of
    --   Ok (e', et) -> case unify t2 et envT1 of
    --     Ok (t', envT2) -> Ok (e', t')
    --     Err err -> Err (err, [App e1 e2])
    --   Err (err, stack) -> Err (err, App e1 e2 : stack)
    Err err -> Err (err, [App e1 e2])
  -- (Ok (Rec x (Lam y e)), Ok e2') -> Ok (App (Rec x (Lam y e)) e2')
  -- (Ok (Rec x e1'), Ok (Rec x' e2')) | x == x' -> Ok (Rec x (App e1' e2'))
  (Ok (Rec x e1', tt), Ok (e2', e2t)) -> Ok (Rec x (App e1' e2'), tt)
  -- (Ok e1', Ok (Rec x e2')) -> Ok (Rec x (App e1' e2'))
  -- (Ok (App Add (Num k1)), Ok (Num k2)) -> Ok (Num (k1 + k2))
  -- (Ok (App Sub (Num k1)), Ok (Num k2)) -> Ok (Num (k1 - k2))
  -- (Ok (App Mul (Num k1)), Ok (Num k2)) -> Ok (Num (k1 * k2))
  -- (Ok (App Eq (Num k1)), Ok (Num k2)) | k1 == k2 -> Ok (Lam "True" (Lam "False" (Var "True")))
  -- (Ok (App Eq (Num k1)), Ok (Num k2)) -> Ok (Lam "True" (Lam "False" (Var "False")))
  -- (Ok e1', Ok e2') -> Ok (App e1' e2')
  (Ok (e1', FunT t1 t2), Ok (e2', t1')) -> case unify t1 t1' envT of
    Ok _ -> Ok (App e1' e2', t2)
    Err err -> Err (err, [App e1 e2])
  (Ok (e1', t), Ok _) -> Err (NotAFunction e1' t, [App e1 e2])
  (Err (err, stack), _) -> Err (err, App e1 e2 : stack)
  (_, Err (err, stack)) -> Err (err, App e1 e2 : stack)
reduce (Rec x (Var x')) _ _ | x == x' = Ok (Rec x (Var x), AnyT x)
reduce (Rec x e) env envT = reduce e ((x, Rec x (Var x)) : env) envT
reduce (Ann e t) env envT = case reduce e env envT of
  Ok (e', et) -> case unify et t envT of
    Ok (t', _) -> Ok (e', t')
    Err err -> Err (err, [Ann e t])
  Err (err, stack) -> Err (err, Ann e t : stack)
reduce Add _ _ = Ok (Add, FunT NumT (FunT NumT NumT))
reduce Sub _ _ = Ok (Sub, FunT NumT (FunT NumT NumT))
reduce Mul _ _ = Ok (Mul, FunT NumT (FunT NumT NumT))
reduce Eq _ _ = Err (UndefinedVar "Not implemented: reduce Eq", []) --Ok (Eq, FunT NumT (FunT NumT (FunT (VarT "a") (VarT "a"))))
