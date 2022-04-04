module PatternMatching where

-- https://www.cas.mcmaster.ca/~kahl/PMC/

{-
TODO:
- Add more descriptive error messages
- Define a human-readable syntax and create a parser and writer
- Communicate reduced expressions through Err for alternations
-}

data Expr
  = Err
  | Var String
  | Ctr String
  | Let (String, Expr) Expr
  | Lam Pattern Expr
  | Or Expr Expr
  | App Expr Expr
  deriving (Eq, Show)

type Pattern = Expr

occurs :: String -> Expr -> Bool
occurs x (Var x') | x == x' = True
occurs x (Let (y, _) a) | x /= y = occurs x a
occurs x (Lam p a) | not (occurs x p) = occurs x a
occurs x (Or a b) | occurs x a || occurs x b = True
occurs x (App a b) | occurs x a || occurs x b = True
occurs _ _ = False

reduce :: Expr -> Expr
reduce (Let (x, a) (Var x')) | x == x' = reduce a
reduce (Let var (Let var' a)) = reduce (Let var (reduce (Let var' a)))
reduce (Let (x, _) (Lam p a)) | occurs x p = Lam p a
reduce (Let var (Lam p a)) = Lam p (Let var a)
reduce (Let var (Or a b)) = Or (Let var a) (Let var b)
reduce (Let var (App a b)) = reduce (App (Let var a) (Let var b))
reduce (Let _ a) = a
reduce (Or a _) = reduce a
reduce (App a b) = case reduce a of
  Err -> Err
  Lam Err a -> if reduce b == Err then reduce a else Err
  Lam (Var x) a -> reduce (Let (x, b) a)
  Lam (Ctr c) a -> case reduce b of
    Ctr c' | c == c' -> reduce a
    _ -> Err
  Lam (Let var p) a -> let p' = reduce (Let var p) in reduce (App (Lam p' a) b)
  -- TODO: Lam
  Lam (Or p1 p2) a -> case reduce (App (Lam p1 a) b) of
    Err -> reduce (App (Lam p2 a) b)
    result -> result
  Lam (App p1 p2) a -> case reduce b of
    App b1 b2 -> reduce (App (Lam p1 (App (Lam p2 a) b2)) b1)
    _ -> Err
  a -> App a b
reduce a = a
