module Core where

import Parser (Parser, alphanumeric, char, identifier, inbetween, infixL, infixR, integer, letter, oneOrMore, prefix, spaces, succeed, term, text)
import qualified Parser

data Expr
  = Any
  | Tup
  | Add
  | Sub
  | Mul
  | IntT
  | Int Int
  | Var String
  | For String Expr
  | Let [(String, Expr)] Expr
  | Or Expr Expr
  | Ann Expr Typ
  | Lam Pattern Expr
  | App Expr Expr
  deriving (Eq, Show)

type Typ = Expr

type Pattern = Expr

type Env = [(String, Expr)]

data Error
  = SyntaxError Parser.Error
  | UndefinedVariable String
  | PatternMismatch Pattern (Expr, Env)
  deriving (Eq, Show)

-- Reduction rules (Weak Head Normal Form)
reduce :: Expr -> Env -> Either Error (Expr, Env)
reduce (Var x) [] = Left (UndefinedVariable x)
reduce (Var x) ((x', a) : env) | x == x' && a == Var x = Right (Var x, (x, Var x) : env)
reduce (Var x) ((x', a) : env) | x == x' = do
  (a, env) <- reduce a env
  Right (a, (x, a) : env)
reduce (Var x) (var : env) = do
  (a, env) <- reduce (Var x) env
  Right (a, var : env)
reduce (For x a) ((x', _) : env) | x == x' = reduce a ((x, Var x) : env)
reduce (For x a) env = reduce a ((x, Var x) : env)
reduce (Let env a) env' = do
  (a, _) <- reduce a env
  Right (a, env')
reduce (App (Or a b) arg) env = case reduce (App a arg) env of
  Left (PatternMismatch _ (arg, env)) -> reduce (App b arg) env
  result -> result
reduce (App (Lam pattern body) arg) env = do
  (env, _) <- match (pattern, env) (arg, env)
  reduce body env
reduce (App (App op a) b) env | op `elem` [Add, Sub, Mul] = do
  (a, env) <- reduce a env
  (b, env) <- reduce b env
  case (op, a, b) of
    (Add, Int i, Int j) -> Right (Int (i + j), env)
    (Sub, Int i, Int j) -> Right (Int (i - j), env)
    (Mul, Int i, Int j) -> Right (Int (i * j), env)
    _ -> Right (App (App op a) b, env)
reduce (App a b) env = do
  (a, env) <- reduce a env
  case a of
    Or _ _ -> reduce (App a b) env
    Lam _ _ -> reduce (App a b) env
    App Add _ -> reduce (App a b) env
    App Sub _ -> reduce (App a b) env
    App Mul _ -> reduce (App a b) env
    _ -> Right (App a b, env)
reduce a env = Right (a, env)

-- Evaluation (Normal Form)
eval :: Expr -> Env -> Either Error (Expr, Env)
eval a env = do
  (a, env) <- reduce a env
  case a of
    Or a b -> do
      (a, env) <- eval a env
      (b, env) <- eval b env
      Right (Or a b, env)
    Ann a _ -> eval a env
    Lam a b -> do
      (a, env) <- eval a env
      (b, env) <- eval b env
      Right (Lam a b, env)
    App a b -> do
      (a, env) <- eval a env
      (b, env) <- eval b env
      Right (App a b, env)
    _ -> Right (a, env)

-- Pattern matching
match :: (Pattern, Env) -> (Expr, Env) -> Either Error (Env, Env)
match (Let cls p, env) (a, env') = do
  (p, cls) <- reduce p cls
  (_, env') <- match (p, cls) (a, env')
  Right (env, env')
match (Or p q, env) aa = case match (p, env) aa of
  Left (PatternMismatch _ aa) -> match (q, env) aa
  result -> result
match pp (Or a b, env') = case match pp (a, env') of
  Left (PatternMismatch _ (_, env')) -> match pp (b, env')
  result -> result
match (Ann p q, env) (Ann a b, env') = do
  (env, env') <- match (p, env) (a, env')
  match (q, env) (b, env')
match (Ann p q, env) (a, env') = do
  b <- typeOf a env'
  match (Ann p q, env) (Ann a b, env')
match (Lam p q, env) (Lam a b, env') = do
  (env, env') <- match (p, env) (a, env')
  match (q, env) (b, env')
match (App p q, env) (App a b, env') = do
  (env, env') <- match (p, env) (a, env')
  match (q, env) (b, env')
-- TODO: simplify these into top level rules
match (p, env) (a, env') = do
  (p, env) <- reduce p env
  case (p, env) of
    (Any, env) -> Right (env, env')
    (Var x, (x', p) : env) | x == x' && p == Var x -> Right ((x, Let env' a) : env, env')
    (Var x, (x', p) : env) | x == x' -> match (p, (x, a) : env) (a, env')
    (Var x, var : env) -> do
      (env, env') <- match (Var x, env) (a, env')
      Right (var : env, env')
    _ -> do
      (a, env') <- reduce a env'
      case a of
        Or _ _ -> match (p, env) (a, env')
        Lam _ _ -> match (p, env) (a, env')
        App _ _ -> match (p, env) (a, env')
        _ | p == a -> Right (env, env')
        _ -> Left (PatternMismatch p (a, env'))

-- Type checking
typeOf :: Expr -> Env -> Either Error Typ
typeOf Any _ = Right Any
typeOf (Int _) _ = Right IntT
typeOf a _ = Left (UndefinedVariable $ "Not implemented!" ++ show a)

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

-- Parsers
expression :: Parser Expr
expression = do
  let name :: Parser String
      name = identifier letter [alphanumeric, char '_', char '\'']
  let binding :: Parser (String, Expr)
      binding = do
        x <- name
        _ <- spaces
        _ <- char '='
        _ <- spaces
        a <- expression
        _ <- spaces
        _ <- char ';'
        _ <- spaces
        succeed (x, a)
  Parser.expression
    [ term (const Any) (char '_'),
      term (const Tup) (text "()"),
      term (const Add) (text "(+)"),
      term (const Sub) (text "(-)"),
      term (const Mul) (text "(*)"),
      term (const IntT) (text "%I"),
      term Int integer,
      prefix For (do _ <- char '@'; name),
      prefix Let (oneOrMore binding),
      term Var name,
      inbetween (const id) (char '(') (char ')')
    ]
    [ infixR 1 (const Or) (char '|'),
      infixR 2 (const Ann) (char ':'),
      infixR 3 (const Lam) (text "->"),
      infixL 4 (const add) (char '+'),
      infixL 4 (const sub) (char '-'),
      infixL 5 (const mul) (char '*'),
      infixL 6 (const App) (succeed ())
    ]

parse :: String -> Either Error Expr
parse text = case Parser.parse text expression of
  Right expr -> Right expr
  Left err -> Left (SyntaxError err)
