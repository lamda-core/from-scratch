module Typed where

import Data.Char (chr, ord)
import Data.Map (Map, member, (!?))
import qualified Data.Map as Map
import Parser (Parser, alphanumeric, char, infixL, infixR, integer, letter, oneOf, oneOrMore, prefix, space, succeed, term, text, zeroOrMore)
import qualified Parser

data Expr
  = Any
  | Tup
  | IntT
  | Int Int
  | Var String
  | Typ [String]
  | Or [Expr]
  | For String Expr
  | Ann Expr Typ
  | Lam Pattern Expr
  | App Expr Expr
  | Add
  | Sub
  | Mul
  deriving (Eq, Show)

binaryOperators :: [Expr]
binaryOperators = [Add, Sub, Mul]

type Typ = Expr

type Pattern = Expr

type Substitution = Expr -> Expr

data Env = Env
  { seed :: Int,
    vars :: Map String Expr
  }
  deriving (Eq, Show)

data Error
  = UndefinedName String
  | CannotUnify Expr Expr
  deriving (Eq, Show)

empty :: Env
empty = Env {seed = 1, vars = Map.empty}

fromList :: [(String, Expr)] -> Env
fromList variables = empty {vars = Map.fromList variables}

has :: String -> Env -> Bool
has x env = x `member` vars env

get :: String -> Env -> Maybe Expr
get x env = vars env !? x

set :: String -> Expr -> Env -> Env
set x t env = env {vars = Map.insert x t (vars env)}

app :: Expr -> [Expr] -> Expr
app = foldl App

add :: Expr -> Expr -> Expr
add a b = app Add [a, b]

sub :: Expr -> Expr -> Expr
sub a b = app Sub [a, b]

mul :: Expr -> Expr -> Expr
mul a b = app Mul [a, b]

-- TODO: FIX THIS
defineType :: String -> [Typ] -> [(String, Typ)] -> Env -> Env
defineType name args ctrs env = do
  let kind = foldr Lam (Typ (map fst ctrs)) args
  let defineCtr env' (x, t) = set x t env'
  foldl defineCtr (set name kind env) ctrs

match :: Pattern -> Expr -> Env -> Maybe Env
match Any _ env = Just env
match (Var x) a env = do
  p <- get x env
  if p == Var x then Just (set x a env) else match p a env
match (Or (p : ps)) a env = case match p a env of
  Just env -> Just env
  Nothing -> match (Or ps) a env
match (For x p) a env = match p a (set x (Var x) env)
-- TODO: Ann Expr Typ -- should typecheck
match pattern (Var x) env = do
  a <- get x env
  match pattern a env
match (Lam p q) (Lam a b) env = do
  env <- match p a env
  match q b env
match (App p q) (App a b) env = do
  env <- match p a env
  match q b env
match p a env | p == a = Just env
match _ _ _ = Nothing

eval :: Expr -> Env -> Expr
eval (Var x) env = case get x env of
  Just a -> a
  Nothing -> Var x
eval (Ann a _) env = eval a env
eval (Or (a : _)) env = eval a env
eval (App (Lam pattern body) arg) env = eval (App (Or [Lam pattern body]) arg) env
eval (App (Or []) arg) env = App (Or []) (eval arg env)
eval (App (Or (Lam pattern body : bs)) arg) env = case match pattern arg env of
  Just env -> eval body env
  Nothing -> eval (App (Or bs) arg) env
eval (App (App op a) b) env | op `elem` binaryOperators =
  case (op, eval a env, eval b env) of
    (Add, Int k1, Int k2) -> Int (k1 + k2)
    (Sub, Int k1, Int k2) -> Int (k1 - k2)
    (Mul, Int k1, Int k2) -> Int (k1 * k2)
    (_, a, b) -> App (App op a) b
eval (App a b) env = case eval a env of
  a@(Or _) -> eval (App a b) env
  a@(Lam _ _) -> eval (App a b) env
  a -> App a (eval b env)
eval a _ = a

typecheck :: Expr -> Env -> Either Error (Typ, Env)
typecheck Any env = Right (Any, env)
typecheck Tup env = Right (Tup, env)
typecheck IntT env = Right (Typ [], env)
typecheck (Int _) env = Right (IntT, env)
typecheck (Var x) env = case get x env of
  Just (Var x') | x == x' -> Right (Var x, env)
  Just (Ann (Var x') t) | x == x' -> do
    (_, env) <- typecheck t env
    Right (instantiate t env)
  Just a -> typecheck a env
  Nothing -> Left (UndefinedName x)
typecheck (Typ _) env = Right (Typ [], env)
typecheck (Or []) env = Right (Any, env)
typecheck (Or (a : bs)) env = do
  (ta, env) <- typecheck a env
  (tb, env) <- typecheck (Or bs) env
  unify ta tb env
typecheck (For x a) env = typecheck a (set x (Var x) env)
typecheck (Ann a t) env = do
  (ta, env) <- typecheck a env
  unify t ta env
typecheck (Lam p a) env = do
  (tp, env) <- typecheck p env
  (ta, env) <- typecheck a env
  Right (Lam tp ta, env)
typecheck (App a b) env = do
  (ta, env) <- typecheck a env
  (tb, env) <- typecheck b env
  let (x, env') = newVar env
  (_, env') <- unify (eval ta env') (Lam tb (Var x)) env'
  Right (eval (Var x) env', env)
typecheck Add env = Right (instantiate (For "a" $ Lam (Var "a") $ Lam (Var "a") (Var "a")) env)
typecheck Sub env = Right (instantiate (For "a" $ Lam (Var "a") $ Lam (Var "a") (Var "a")) env)
typecheck Mul env = Right (instantiate (For "a" $ Lam (Var "a") $ Lam (Var "a") (Var "a")) env)

unify :: Expr -> Expr -> Env -> Either Error (Expr, Env)
unify Any b env = Right (b, env)
unify a Any env = Right (a, env)
unify (For x a) b env = unify a b (set x (Var x) env)
unify a (For x b) env = unify a b (set x (Var x) env)
unify (Ann a ta) (Ann b tb) env = unify2 Ann (a, ta) (b, tb) env
unify (Lam a1 b1) (Lam a2 b2) env = unify2 Lam (a1, b1) (a2, b2) env
unify a a' env | a == a' = Right (a, env)
unify (Var x) b _ | x `occurs` b = Left (CannotUnify (Var x) b)
unify a (Var x) _ | x `occurs` a = Left (CannotUnify a (Var x))
unify (Var x) b env = Right (b, set x b env)
unify a (Var x) env = Right (a, set x a env)
unify a b _ = Left (CannotUnify a b)

unify2 :: (Expr -> Expr -> Expr) -> (Expr, Expr) -> (Expr, Expr) -> Env -> Either Error (Expr, Env)
unify2 f (a1, b1) (a2, b2) env = do
  (a, env) <- unify a1 a2 env
  (b, env) <- unify b1 b2 env
  Right (f (eval a env) b, env)

occurs :: String -> Expr -> Bool
occurs x (Var x') = x == x'
occurs x (Ann a b) = occurs x a || occurs x b
occurs x (Lam a b) = occurs x a || occurs x b
-- occurs x (Lam ((p, a) : alts)) = (not (occurs x p) && occurs x a) || occurs x (Lam alts)
occurs x (App a b) = occurs x a || occurs x b
occurs _ _ = False

instantiate :: Expr -> Env -> (Expr, Env)
instantiate (For x a) env | has x env = do
  let (y, env') = newVar env
  rename x y a env'
instantiate (For x a) env = instantiate a (set x (Var x) env)
instantiate (Ann a t) env = instantiate2 Ann a t env
instantiate (Lam a b) env = instantiate2 Lam a b env
instantiate (App a b) env = instantiate2 App a b env
-- TODO: Lam -- does it make sense?
instantiate a env = (a, env)

instantiate2 :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Env -> (Expr, Env)
instantiate2 f a b env = do
  let (a', env1) = instantiate a env
  let (b', env2) = instantiate b env1
  (f a' b', env2)

rename :: String -> String -> Expr -> Env -> (Expr, Env)
rename x y (Var x') env | x == x' = (Var y, env)
rename x y (For y' a) env | y == y' = do
  let (z, env1) = newVar env
  let (a', env2) = rename y z a env1
  rename x y (For z a') env2
rename x y (For z a) env | x /= z = do
  let (a', env') = rename x y a env
  (For z a', env')
rename x y (Ann a t) env = rename2 x y Ann a t env
rename x y (Lam a b) env = rename2 x y Lam a b env
rename x y (App a b) env = rename2 x y App a b env
-- TODO: Lam -- does it make sense?
rename _ _ a env = (a, env)

rename2 :: String -> String -> (Expr -> Expr -> Expr) -> Expr -> Expr -> Env -> (Expr, Env)
rename2 x y f a b env = do
  let (a', env1) = rename x y a env
  let (b', env2) = rename x y b env1
  (f a' b', env2)

newVar :: Env -> (String, Env)
newVar env = do
  let x = intToName (seed env)
  let env' = env {seed = seed env + 1}
  if has x env
    then newVar env'
    else (x, set x (Var x) env')

intToName :: Int -> String
intToName 0 = ""
intToName i = do
  let (q, r) = quotRem (i - 1) 26
  chr (r + ord 'a') : intToName q

expression :: Parser Expr
expression = do
  let name :: Parser String
      name = do
        x <- letter
        xs <- zeroOrMore (oneOf [alphanumeric, char '_', char '\''])
        succeed (x : xs)
  Parser.expression
    [ term (const Any) (char '_'),
      term (const IntT) (text "$Int"),
      term Int integer,
      term Var name,
      term Typ (do _ <- text "$Type"; zeroOrMore (do _ <- char '!'; name)),
      term Or (oneOrMore (do _ <- char '|'; expression)),
      term (const Tup) (text "()"),
      term (const Add) (text "(+)"),
      term (const Sub) (text "(-)"),
      term (const Mul) (text "(*)"),
      prefix For (do _ <- char '@'; x <- name; _ <- char '.'; succeed x)
    ]
    [ infixR 1 (const Ann) (char ':'),
      infixR 2 (const Lam) (text "->"),
      infixL 3 (const add) (text "+"),
      infixL 3 (const sub) (text "-"),
      infixL 4 (const mul) (text "*"),
      infixL 5 (const App) (oneOrMore space)
    ]

-- alternatives :: Typ -> Env -> Either Error [Pattern]
-- alternatives (Typ (x : xs)) env = do
--   (t, _) <- typecheck (Var x) env
--   alts <- alternatives (Typ xs) env
--   Right (PCtr x (replicate (arity t) PAny) : alts)
-- alternatives (Typ []) _ = Right []
-- alternatives (Var x) env = do
--   (kind, _) <- typecheck (Var x) env
--   alternatives kind env
-- alternatives (App t _) env = alternatives t env
-- alternatives (Fun _ t) env = alternatives t env
-- alternatives _ _ = Right [PAny]

-- specialize :: [Pattern] -> Env -> Either Error [Pattern]
-- specialize (PCtr x args : ps) env = do
--   (t, _) <- typecheck (Var x) env
--   alts <- alternatives t env
--   cases <- specialize ps env
--   Right (alts <> cases)
-- specialize [] _ = Right []
-- specialize _ _ = Right [PAny]

-- returnType :: Typ -> Typ
-- returnType (TFun _ t) = returnType t
-- returnType t = t

-- specialize :: Pattern -> Env -> Either Error [Pattern]
-- specialize (PCtr x ps) env = do
--   (typ, _) <- check (Var x) env
--   (kind, _) <- checkT (returnType typ) env
--   case kind of
--     TTyp alts -> specializeCtr alts x ps env
--     _ -> Right [PAny]
-- specialize (PTup ps) env = do
--   cases <- specializeProduct ps env
--   Right (map PTup cases)
-- specialize _ _ = Right [PAny]

-- specializeCtr :: [String] -> String -> [Pattern] -> Env -> Either Error [Pattern]
-- specializeCtr (x : alts) x' ps env | x == x' = do
--   (t, _) <- check (Var x) env
--   if length ps == arity t
--     then do
--       args <- specializeProduct ps env
--       cases <- specializeCtr alts x ps env
--       Right (map (PCtr x) args <> cases)
--     else Left (NumArgsMismatch {ctr = x, expected = arity t, got = length ps})
-- specializeCtr (x : alts) y ps env = do
--   (t, _) <- check (Var x) env
--   cases <- specializeCtr alts y ps env
--   Right (PCtr x (replicate (arity t) PAny) : cases)
-- specializeCtr [] _ _ _ = Right []

-- specializeProduct :: [Pattern] -> Env -> Either Error [[Pattern]]
-- specializeProduct (p : qs) env = do
--   ps <- specialize p env
--   qss <- specializeProduct qs env
--   Right (concatMap (\p -> map (p :) qss) ps)
-- specializeProduct [] _ = Right [[]]

-- arity :: Typ -> Int
-- arity (Fun _ b) = 1 + arity b
-- arity _ = 0
