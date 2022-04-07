module Core where

{-
Referenes:
- Pattern Matching Calculus: Lamdas with arbitrary patterns
  https://www.cas.mcmaster.ca/~kahl/PMC/
- Pure Pattern Calculus: Application as a pattern
  https://link.springer.com/content/pdf/10.1007%2F11693024_8.pdf
- Closure Calculus: Environment as part of expressions
  https://blog.chewxy.com/wp-content/uploads/personal/dissertation31482.pdf

TODO:
- Add more descriptive error messages
- Define a human-readable syntax and create a parser and writer
-}

data Expr
  = Err -- TODO: keep the reduced argument to only evaluate it once
  | Tup
  | Add
  | Sub
  | Mul
  | Int Int
  | Var String
  | Ctr String
  | Let Env Expr
  | Lam Pattern Expr
  | Or Expr Expr
  | App Expr Expr
  deriving (Eq, Show)

type Pattern = Expr

type Env = [(String, Expr)]

get :: String -> Env -> Expr
get _ [] = Err
get x ((x', a) : _) | x == x' = a
get x (_ : env) = get x env

occurs :: String -> Expr -> Bool
occurs x (Var x') | x == x' = True
occurs x (Let [] a) = occurs x a
occurs x (Let ((y, _) : env) a) | x /= y = occurs x (Let env a)
occurs x (Lam p a) | not (occurs x p) = occurs x a
occurs x (Or a b) | occurs x a || occurs x b = True
occurs x (App a b) | occurs x a || occurs x b = True
occurs _ _ = False

reduce :: Expr -> Expr
reduce (Let env (Var x)) = reduce (Let env (get x env))
reduce (Let env (Let env' a)) = reduce (Let (env ++ env') a)
reduce (Let env (Lam p a)) = Lam p (Let env a)
reduce (Let env (Or a b)) = Or (Let env a) (Let env b)
reduce (Let env (App a b)) = reduce (App (Let env a) (Let env b))
reduce (Let _ a) = a
reduce (App a b) = case reduce a of
  Err -> Err
  Lam (Var x) a -> reduce (Let [(x, b)] a)
  Lam (Let var p) a -> let p' = reduce (Let var p) in reduce (App (Lam p' a) b)
  -- TODO: Lam
  Lam (Or p1 p2) a -> reduce (App (Or (Lam p1 a) (Lam p2 a)) b)
  Lam (App p1 p2) a -> case reduce b of
    App b1 b2 -> reduce (App (Lam p1 (App (Lam p2 a) b2)) b1)
    _ -> Err
  Lam p a | p == reduce b -> reduce a
  Lam _ _ -> Err
  Or a1 a2 -> case reduce (App a1 b) of
    Err -> reduce (App a2 b)
    result -> result
  App op a | op `elem` [Add, Sub, Mul] -> case (op, reduce a, reduce b) of
    (Add, Int a, Int b) -> Int (a + b)
    (Sub, Int a, Int b) -> Int (a - b)
    (Mul, Int a, Int b) -> Int (a * b)
    (op, a, b) -> App (App op a) b
  a -> App a b
reduce a = a

-- Helper functions / syntax sugar
lam :: [Expr] -> Expr -> Expr
lam xs a = foldr Lam a xs

app :: Expr -> [Expr] -> Expr
app = foldl App

add :: Expr -> Expr -> Expr
add a b = app Add [a, b]

sub :: Expr -> Expr -> Expr
sub a b = app Sub [a, b]

mul :: Expr -> Expr -> Expr
mul a b = app Mul [a, b]
