module Core where

import Data.List (elemIndex, intercalate)
import Parser (Parser, alphanumeric, chain, char, identifier, inbetween, infixL, infixR, integer, letter, oneOrMore, prefix, prefixList, spaces, succeed, term, text)
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
  | Let (String, Expr) Expr
  | Or Expr Expr
  | Ann Expr Typ
  | Lam Pattern Expr
  | App Expr Expr
  deriving (Eq, Show)

-- instance Show Expr where
--   show expr =
--     let infixL prec a op b | precedenceOf b <= prec = show' a ++ op ++ "(" ++ show' b ++ ")"
--         infixL _ a op b = show' a ++ op ++ show' b

--         infixR prec a op b | precedenceOf a <= prec = "(" ++ show' a ++ ")" ++ op ++ show' b
--         infixR _ a op b = show' a ++ op ++ show' b

--         show' :: Expr -> String
--         show' Any = "_"
--         show' Tup = "()"
--         show' Add = "(+)"
--         show' Sub = "(-)"
--         show' Mul = "(*)"
--         show' IntT = "%Int"
--         show' (Int i) = show i
--         show' (Var x) = x
--         show' (Or a b) = infixR precOr a " | " b
--         show' (Ann a b) = infixR precAnn a " : " b
--         show' (Lam a b) = infixR precLam a " -> " b
--         show' (App (App Add a) b) = infixL precAdd a " + " b
--         show' (App (App Sub a) b) = infixL precSub a " - " b
--         show' (App (App Mul a) b) = infixL precMul a " * " b
--         show' (App a b) = infixL precApp a " " b
--         show' (Let [] b) = show b
--         show' (Let ((x, a) : vars) b) = x ++ " = " ++ show' a ++ "; " ++ show' (Let vars b)
--      in show' expr

type Typ = Expr

type Pattern = Expr

type Env = [(String, Expr)]

data Error
  = SyntaxError Parser.Error
  | UndefinedVariable String
  | PatternMismatch Pattern Expr
  deriving (Eq, Show)

-- Helper functions
let' :: [(String, Expr)] -> Expr -> Expr
let' env a = foldr Let a env

-- lam :: [Expr] -> Expr -> Expr
-- lam = foldr

app :: Expr -> [Expr] -> Expr
app = foldl App

add :: Expr -> Expr -> Expr
add a b = app Add [a, b]

sub :: Expr -> Expr -> Expr
sub a b = app Sub [a, b]

mul :: Expr -> Expr -> Expr
mul a b = app Mul [a, b]

-- -- Operator precedence definitions
-- -- TODO: anonymous records like Elm would make this nicer.
-- -- TODO: make this into a Grammar type and allow to parse and show
-- precOr :: Int
-- precOr = 1

-- precAnn :: Int
-- precAnn = 2

-- precLam :: Int
-- precLam = 3

-- precAdd :: Int
-- precAdd = 4

-- precSub :: Int
-- precSub = 4

-- precMul :: Int
-- precMul = 5

-- precApp :: Int
-- precApp = 6

-- precTerm :: Int
-- precTerm = 7

-- precedenceOf :: Expr -> Int
-- precedenceOf (Or _ _) = precOr
-- precedenceOf (Ann _ _) = precAnn
-- precedenceOf (Lam _ _) = precLam
-- precedenceOf (App (App Add _) _) = precAdd
-- precedenceOf (App (App Sub _) _) = precSub
-- precedenceOf (App (App Mul _) _) = precMul
-- precedenceOf (App _ _) = precApp
-- precedenceOf _ = precTerm

-- -- Parsers
-- -- TODO: Make this something easy/fast to parse without operator precedence.
-- --       All the syntax sugar should come from a high level language.
-- expression :: Parser Expr
-- expression = do
--   let name :: Parser String
--       name = identifier letter [alphanumeric, char '_', char '\'']
--   let binding :: Parser (String, Expr)
--       binding = do
--         x <- name
--         _ <- spaces
--         _ <- char '='
--         _ <- spaces
--         a <- expression
--         _ <- spaces
--         _ <- char ';'
--         _ <- spaces
--         succeed (x, a)
--   Parser.expression
--     [ term (const Any) (char '_'),
--       term (const Tup) (chain [text "(", spaces, text ")"]),
--       term (const Add) (chain [text "(", spaces, text "+", spaces, text ")"]),
--       term (const Sub) (chain [text "(", spaces, text "-", spaces, text ")"]),
--       term (const Mul) (chain [text "(", spaces, text "*", spaces, text ")"]),
--       term (const IntT) (text "%Int"),
--       term Int integer,
--       prefix Env binding,
--       term Var name,
--       inbetween (const id) (char '(') (char ')')
--     ]
--     [ infixR precOr (const Or) (char '|'),
--       infixR precAnn (const Ann) (char ':'),
--       infixR precLam (const Lam) (text "->"),
--       infixL precAdd (const add) (char '+'),
--       infixL precSub (const sub) (char '-'),
--       infixL precMul (const mul) (char '*'),
--       infixL precApp (const App) (succeed ())
--     ]

-- parse :: String -> Either Error Expr
-- parse text = case Parser.parse text expression of
--   Right expr -> Right expr
--   Left err -> Left (SyntaxError err)

-- -- Reduction rules
-- reduce :: Expr -> Env -> Either Error (Expr, Env)
-- reduce (Var x) [] = Left (UndefinedVariable x)
-- reduce (Var x) ((x', a) : env) | x == x' = do
--   if a == Var x
--     then Right (Var x, (x, Var x) : env)
--     else do
--       (a, env) <- reduce a env
--       Right (a, (x, a) : env)
-- reduce (Var x) (var : env) = do
--   (a, env) <- reduce (Var x) env
--   Right (a, var : env)
-- -- reduce (Let (x, Any) a) env = reduce a ((x, Var x) : env)
-- -- reduce (Let var (Let (x, a) b)) env = do
-- --   (b, env) <- reduce (Let var b) ((x, Let var a) : env)
-- -- -- Right (Let (x, Let var a) (Let var b), env)
-- -- reduce (Let var (Or a b)) env = Right (Or (Let var a) (Let var b), env)
-- -- reduce (Let var (Ann a b)) env = Right (Ann (Let var a) (Let var b), env)
-- reduce (Let _ a) env = reduce a env
-- reduce (App (Or a b) arg) env = case reduce (App a arg) env of
--   Left (PatternMismatch _ arg) -> reduce (App b arg) env
--   result -> result
-- reduce (App (Lam pattern body) arg) env = do
--   (pattern, env) <- eval pattern env
--   env <- match pattern arg env
--   reduce body env
-- reduce (App (App op a) b) env | op `elem` [Add, Sub, Mul] = do
--   (a, env) <- reduce a env
--   (b, env) <- reduce b env
--   case (op, a, b) of
--     (Add, Int i, Int j) -> Right (Int (i + j), env)
--     (Sub, Int i, Int j) -> Right (Int (i - j), env)
--     (Mul, Int i, Int j) -> Right (Int (i * j), env)
--     _ -> Right (App (App op a) b, env)
-- reduce (App a b) env = do
--   (a, env) <- reduce a env
--   case a of
--     Or _ _ -> reduce (App a b) env
--     Lam _ _ -> reduce (App a b) env
--     App Add _ -> reduce (App a b) env
--     App Sub _ -> reduce (App a b) env
--     App Mul _ -> reduce (App a b) env
--     _ -> Right (App a b, env)
-- reduce a env = Right (a, env)

-- Reduction rules
reduce :: Expr -> Env -> Either Error (Expr, Env)
reduce (Var x) [] = Left (UndefinedVariable x)
reduce (Var x) ((x', a) : env) | x == x' = do
  if a == Var x
    then Right (Var x, (x, Var x) : env)
    else do
      (a, env) <- reduce a env
      Right (a, (x, a) : env)
reduce (Var x) (var : env) = do
  (a, env) <- reduce (Var x) env
  Right (a, var : env)
reduce (Let var a) env = reduce a (var : env)
reduce (App (Or a b) arg) env = case reduce (App a arg) env of
  Left (PatternMismatch _ arg) -> reduce (App b arg) env
  result -> result
reduce (App (Lam pattern body) arg) env = do
  (pattern, env) <- eval pattern env
  env <- match pattern arg env
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

-- Pattern matching
match :: Pattern -> Expr -> Env -> Either Error Env
match Any _ env = Right env
match (Var x) a ((x', _) : env) | x == x' = Right ((x, a) : env)
match (Var x) a (var : env) = do
  env <- match (Var x) a env
  Right (var : env)
match (Or p q) a env = case match p a env of
  Left (PatternMismatch _ a) -> match q a env
  result -> result
match p (Or a b) env = case match p a env of
  Left (PatternMismatch _ _) -> match p b env
  result -> result
match (Ann p q) (Ann a b) env = do
  env <- match p a env
  match q b env
match (Ann p q) a env = do
  b <- typeOf a
  match (Ann p q) (Ann a b) env
match (Lam p q) (Lam a b) env = do
  env <- match p a env
  match q b env
match (App p q) (App a b) env = do
  env <- match p a env
  match q b env
match p a env = do
  (a, closure) <- reduce a env
  case a of
    Or _ _ -> match p (let' closure a) env
    Lam a b -> match p (Lam (let' closure a) (let' closure b)) env
    App a b -> match p (App (let' closure a) (let' closure b)) env
    _ | p == a -> Right env
    _ -> Left (PatternMismatch p a)

typeOf :: Expr -> Either Error Typ
typeOf Any = Right Any
typeOf (Ann _ t) = Right t -- TODO: type check a
typeOf (Int _) = Right IntT

{- Roadmap
* benchmark factorial and ackermann
* Code generation into Javascript
-}

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
