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
  = Err Expr
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
get x [] = Err (Var x)
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

-- TODO: remove explicit Env, all should be part of Let as closures
reduce :: Expr -> Env -> (Expr, Env)
reduce (Var x) env = reduce (get x env) env -- TODO: update env
reduce (Let env a) env' = do
  let (a', _) = reduce a env
  (a', env')
reduce (Or a b) env = case reduce a env of -- TODO: tests
  (Err _, env) -> reduce b env
  (Lam p a, env) -> (Or (Lam p a) b, env)
  (a, env) -> (a, env)
reduce (App a b) env = case reduce a env of
  (Err a, env) -> (Err (App a b), env)
  (Lam (Err _) a, env) -> case reduce b env of
    (Err _, env) -> reduce a env
    result -> result
  (Lam (Var x) a, env) -> do
    let (a', env') = reduce a ((x, Let env b) : env)
    (a', tail env')
  (Lam (Let var p) a, env) -> do
    let (p', _) = reduce (Let var p) env
    reduce (App (Lam p' a) b) env
  -- TODO: Lam
  (Lam (Or p1 p2) a, env) -> reduce (App (Or (Lam p1 a) (Lam p2 a)) b) env
  (Lam (App p1 p2) a, env) -> case reduce b env of
    (App b1 b2, env) -> reduce (App (Lam p1 (App (Lam p2 a) b2)) b1) env
    (b, env) -> (Err b, env)
  (Lam p a, env) -> case reduce b env of
    (b, env) | p == b -> reduce a env
    (b, env) -> (Err b, env)
  (Or a1 a2, env) -> reduce (Or (App a1 b) (App a2 b)) env
  (App op a, env) | op `elem` [Add, Sub, Mul] -> do
    let (a', env1) = reduce a env
    let (b', env2) = reduce b env1
    case (op, a', b') of
      (Add, Int a, Int b) -> (Int (a + b), env2)
      (Sub, Int a, Int b) -> (Int (a - b), env2)
      (Mul, Int a, Int b) -> (Int (a * b), env2)
      (op, a, b) -> (App (App op a) b, env2)
  (a, env) -> (App a b, env)
reduce a env = (a, env)

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
