module DeBruijn where

import Data.List (elemIndex, find)
import Parser (Parser, alphanumeric, chain, char, identifier, inbetween, infixL, infixR, integer, letter, prefix, prefixList, spaces, succeed, term, text)
import qualified Parser

-- TODO: Create an AST with all the syntax sugar

data Expr
  = Any
  | Tup
  | Add
  | Sub
  | Mul
  | IntT
  | Int Int
  | Var Int
  | For String Expr
  | Or Expr Expr
  | Ann Expr Typ
  | Lam Pattern Expr
  | App Expr Expr
  deriving (Eq)

instance Show Expr where
  show expr =
    let infixL prec a op b vars | precedenceOf b <= prec = show' a vars ++ op ++ "(" ++ show' b vars ++ ")"
        infixL _ a op b vars = show' a vars ++ op ++ show' b vars

        infixR prec a op b vars | precedenceOf a <= prec = "(" ++ show' a vars ++ ")" ++ op ++ show' b vars
        infixR _ a op b vars = show' a vars ++ op ++ show' b vars

        show' :: Expr -> [String] -> String
        show' Any _ = "_"
        show' Tup _ = "()"
        show' Add _ = "(+)"
        show' Sub _ = "(-)"
        show' Mul _ = "(*)"
        show' IntT _ = "%Int"
        show' (Int i) _ = show i
        show' (Var 0) (x : _) = x
        show' (Var i) (_ : xs) = show' (Var (i - 1)) xs
        show' (Var i) [] = "$" ++ show i
        show' (For x a) vars = do
          let forAll xs (For x a) = forAll (x : xs) a
              forAll xs a = (xs, a)
          let (xs, a') = forAll [x] a
          "@" ++ unwords (reverse xs) ++ ". " ++ show' a' (xs ++ vars)
        show' (Or a b) vars = infixR precOr a " | " b vars
        show' (Ann a b) vars = infixR precAnn a " : " b vars
        show' (Lam a b) vars = infixR precLam a " -> " b vars
        show' (App (App Add a) b) vars = infixL precAdd a " + " b vars
        show' (App (App Sub a) b) vars = infixL precSub a " - " b vars
        show' (App (App Mul a) b) vars = infixL precMul a " * " b vars
        show' (App a b) vars = infixL precApp a " " b vars
     in show' expr []

type Typ = Expr

type Pattern = Expr

type Env = [(String, Expr)]

data Error
  = SyntaxError Parser.Error
  | UndefinedName String
  deriving (Eq, Show)

-- Operator precedence definitions
-- TODO: anonymous records like Elm would make this nicer.
-- TODO: make this into a Grammar type and allow to parse and show
precOr :: Int
precOr = 1

precAnn :: Int
precAnn = 2

precLam :: Int
precLam = 3

precAdd :: Int
precAdd = 4

precSub :: Int
precSub = 4

precMul :: Int
precMul = 5

precApp :: Int
precApp = 6

precTerm :: Int
precTerm = 7

precedenceOf :: Expr -> Int
precedenceOf (Or _ _) = precOr
precedenceOf (Ann _ _) = precAnn
precedenceOf (Lam _ _) = precLam
precedenceOf (App (App Add _) _) = precAdd
precedenceOf (App (App Sub _) _) = precSub
precedenceOf (App (App Mul _) _) = precMul
precedenceOf (App _ _) = precApp
precedenceOf _ = precTerm

-- Helper functions
app :: Expr -> [Expr] -> Expr
app = foldl App

add :: Expr -> Expr -> Expr
add a b = app Add [a, b]

sub :: Expr -> Expr -> Expr
sub a b = app Sub [a, b]

mul :: Expr -> Expr -> Expr
mul a b = app Mul [a, b]

-- Parsers
expression :: Parser ([String] -> Either Error Expr)
expression = do
  let name = identifier letter [alphanumeric, char '_', char '\'']
  let variable :: String -> [String] -> Either Error Expr
      variable x vars = case elemIndex x vars of
        Just i -> Right (Var i)
        Nothing -> Left (UndefinedName x)
  let withVariables :: [String] -> ([String] -> Either Error Expr) -> [String] -> Either Error Expr
      withVariables xs f vars = do
        expr <- f (foldl (flip (:)) vars xs)
        Right (foldr For expr xs)
  let binary :: (Expr -> Expr -> Expr) -> ([String] -> Either Error Expr) -> ([String] -> Either Error Expr) -> [String] -> Either Error Expr
      binary f fa fb vars = do
        a <- fa vars
        b <- fb vars
        Right (f a b)
  Parser.expression
    [ term (\_ _ -> Right Any) (char '_'),
      term (\_ _ -> Right Tup) (chain [text "(", spaces, text ")"]),
      term (\_ _ -> Right Add) (chain [text "(", spaces, text "+", spaces, text ")"]),
      term (\_ _ -> Right Sub) (chain [text "(", spaces, text "-", spaces, text ")"]),
      term (\_ _ -> Right Mul) (chain [text "(", spaces, text "*", spaces, text ")"]),
      term (\_ _ -> Right IntT) (text "%Int"),
      term (\i _ -> Right (Int i)) integer,
      term variable name,
      prefix withVariables (prefixList (:) [] (char '@') name (char '.')),
      inbetween (const id) (char '(') (char ')')
    ]
    [ infixR precOr (const $ binary Or) (char '|'),
      infixR precAnn (const $ binary Ann) (char ':'),
      infixR precLam (const $ binary Lam) (text "->"),
      infixL precAdd (const $ binary add) (char '+'),
      infixL precSub (const $ binary sub) (char '-'),
      infixL precMul (const $ binary mul) (char '*'),
      infixL precApp (const $ binary App) (succeed ())
    ]

parse :: String -> [String] -> Either Error Expr
parse text vars = case Parser.parse text expression of
  Right expr -> expr vars
  Left err -> Left (SyntaxError err)

-- Reduction rules
eval :: Expr -> Env -> Expr
eval (Var i) env = snd (env !! i) -- TODO: use a Maybe here
eval (For x a) env = eval a ((x, Any) : env)
eval (Or a _) env = eval a env
eval (Ann a _) env = eval a env
eval (Lam a b) env = Lam (eval a env) (eval b env)
eval (App (Lam p a) b) env = eval (App (Or (Lam p a) Any) b) env
eval (App (Or (Lam p a) other) b) env = case match p b env of
  Right env -> eval a env
  Left b -> eval (App other b) env
eval (App (App op a) b) env | op `elem` [Add, Sub, Mul] =
  case (op, eval a env, eval b env) of
    (Add, Int k1, Int k2) -> Int (k1 + k2)
    (Sub, Int k1, Int k2) -> Int (k1 - k2)
    (Mul, Int k1, Int k2) -> Int (k1 * k2)
    (_, a, b) -> App (App op a) b
eval (App a b) env = case eval a env of
  a@(Or _ _) -> eval (App a b) env
  a@(Lam _ _) -> eval (App a b) env
  a -> App a (eval b env)
eval a _ = a

typeOf :: Expr -> Env -> Either Error Typ
typeOf Any _ = Right Any
typeOf (Int _) _ = Right IntT

reduce :: Expr -> Env -> Maybe (Expr, Env)
reduce (Var i) env = reduce (snd (env !! i)) env -- TODO: use a Maybe here
-- reduce (App (For x a) b) env = reduce (App a b) ((x, Any) : env)
reduce (App (Lam pattern body) arg) env = case match pattern arg env of
  Right env -> reduce body env
  Left _ -> Nothing
reduce (App (Or a b) arg) env = case reduce (App a arg) env of
  Just a -> Just a
  Nothing -> reduce (App b arg) env
reduce (App (App op a) b) env | op `elem` [Add, Sub, Mul] = do
  (a, env) <- reduce a env
  (b, env) <- reduce b env
  case (op, a, b) of
    (Add, Int a, Int b) -> Just (Int (a + b), env)
    (Sub, Int a, Int b) -> Just (Int (a - b), env)
    (Mul, Int a, Int b) -> Just (Int (a * b), env)
    _ -> Just (App (App op a) b, env)
reduce (App a b) env = do
  (a, env) <- reduce a env
  case a of
    Lam _ _ -> reduce (App a b) env
    Or _ _ -> reduce (App a b) env
    _ -> Just (App a b, env)
reduce a env = Just (a, env)

match :: Pattern -> Expr -> Env -> Either Expr Env
match Any _ env = Right env
match (Var 0) b ((x, a) : env) = match a b ((x, b) : env)
match (Var i) b (var : env) = fmap (var :) (match (Var (i - 1)) b env)
match (For x a) b env = match a b ((x, Any) : env)
match (Or a1 a2) b env = case match a1 b env of
  Right env -> Right env
  Left b -> match a2 b env
match (Ann a ta) (Ann b tb) env = do
  env <- match a b env
  match ta tb env
match (Ann a ta) b env = case typeOf b env of
  Right tb -> match (Ann a ta) (Ann b tb) env
  Left _ -> Left b
match (Lam a1 a2) b env = case reduce b env of
  Just (Lam b1 b2, env) -> do
    env <- match a1 b1 env
    match a2 b2 env
  Just (b, _) -> Left b
  Nothing -> Left b
match (App a1 a2) b env = case reduce b env of
  Just (App b1 b2, env) -> do
    env <- match a1 b1 env
    match a2 b2 env
  Just (b, _) -> Left b
  Nothing -> Left b
match a b env = case reduce b env of
  Just (b, env) | a == b -> Right env
  Just (b, _) -> Left b
  Nothing -> Left b
