module Core where

import Data.List (delete, find, union)
import Data.Maybe (fromMaybe)
import Parser (Parser)
import qualified Parser as P
import Text.Read (readMaybe)

-- Bidirectional type checking: https://youtu.be/utyBNDj7s2w
-- https://www.cse.iitk.ac.in/users/ppk/teaching/cs653/notes/lectures/Lambda-calculus.lhs.pdf

type Variable = String

type Constructor = String

data Expr
  = Var Variable
  | Int Int
  | App Expr Expr
  | Lam Variable Expr
  | Op2 BinaryOperator
  deriving (Eq, Show)

data BinaryOperator
  = Add
  | Sub
  | Mul
  | Eq
  deriving (Eq, Show)

data Pattern
  = PAny
  | PVar Variable
  | PInt Int
  | PCtr Constructor [Pattern]
  deriving (Eq, Show)

type Context = Constructor -> Maybe [(Constructor, [Variable])]

newtype Error
  = SyntaxError P.Error
  deriving (Eq, Show)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

infixl 1 |>

app :: Expr -> [Expr] -> Expr
app = foldl App

lam :: [Variable] -> Expr -> Expr
lam xs a = foldr Lam a xs

add :: Expr -> Expr -> Expr
add a b = app (Op2 Add) [a, b]

sub :: Expr -> Expr -> Expr
sub a b = app (Op2 Sub) [a, b]

mul :: Expr -> Expr -> Expr
mul a b = app (Op2 Mul) [a, b]

eq :: Expr -> Expr -> Expr
eq a b = app (Op2 Eq) [a, b]

if' :: Expr -> Expr -> Expr -> Expr
if' cond then' else' = app cond [then', else']

-- Constructor alternatives
ctrAlts :: Constructor -> Context -> Maybe [Constructor]
ctrAlts ctr ctx = do
  ctrs <- ctx ctr
  Just (map fst ctrs)

-- Constructor arguments
ctrArgs :: Constructor -> Context -> Maybe [Variable]
ctrArgs ctr ctx = do
  ctrs <- ctx ctr
  (_, xs) <- find (\(c, _) -> c == ctr) ctrs
  Just xs

caseFind :: [(Constructor, [Variable], Expr)] -> Expr -> Constructor -> Expr
caseFind cases default' c = case find (\(c', _, _) -> c == c') cases of
  Just (_, xs, a) -> lam xs a
  Nothing -> default'

case' :: Expr -> [(Constructor, [Variable], Expr)] -> Expr -> Context -> Expr
case' _ [] default' _ = default'
case' a cases@((ctr, _, _) : _) default' ctx = case ctrAlts ctr ctx of
  Just ctrs -> app a (map (caseFind cases default') ctrs)
  Nothing -> default'

nameIndex :: String -> String -> Maybe Int
nameIndex "" x = readMaybe x
nameIndex (c : prefix) (c' : x) | c == c' = nameIndex prefix x
nameIndex _ _ = Nothing

findLastNameIndex :: String -> [String] -> Maybe Int
findLastNameIndex _ [] = Nothing
findLastNameIndex prefix (x : xs) = case findLastNameIndex prefix xs of
  Just i -> case nameIndex prefix x of
    Just j -> Just (max i j)
    Nothing -> Just i
  Nothing -> if prefix == x then Just 0 else nameIndex prefix x

freeVariables :: Expr -> [String]
freeVariables (Var x) = [x]
freeVariables (App a b) = freeVariables a `union` freeVariables b
freeVariables (Lam x a) = delete x (freeVariables a)
freeVariables _ = []

newName :: [String] -> String -> String
newName used x = case findLastNameIndex x used of
  Just i -> x ++ show (i + 1)
  Nothing -> x

newNames :: [String] -> [String] -> [String]
newNames _ [] = []
newNames used (x : xs) = let y = newName used x in y : newNames (y : used) xs

patternName :: Pattern -> Maybe Variable
patternName (PVar x) = Just x
patternName _ = Nothing

renamePatterns :: [Pattern] -> [Variable] -> Expr -> Expr
renamePatterns (PVar x : ps) (y : ys) a | x /= y = substitute x (Var y) (renamePatterns ps ys a)
renamePatterns (_ : ps) (_ : ys) a = renamePatterns ps ys a
renamePatterns _ _ a = a

pathToCase :: Context -> ([Pattern], Expr) -> Maybe (Constructor, [Variable], Expr)
pathToCase ctx (PCtr c ps : _, a) = do
  args <- ctrArgs c ctx
  let usedNames = freeVariables (lam (filterMap patternName ps) a)
  let names = zipWith (\x p -> fromMaybe x (patternName p)) args ps
  let xs = newNames usedNames names
  Just (c, xs, renamePatterns ps xs a)
pathToCase _ _ = Nothing

matchAny :: Expr -> [([Pattern], Expr)] -> [([Pattern], Expr)]
matchAny _ [] = []
matchAny a ((PAny : ps, b) : paths) = (ps, b) : matchAny a paths
matchAny a ((PVar x : ps, b) : paths) = (ps, substitute x a b) : matchAny a paths
matchAny a (_ : paths) = matchAny a paths

match :: [Expr] -> [([Pattern], Expr)] -> Expr -> Context -> Expr
match [] ((_, body) : _) _ _ = body
match args [] default' _ = lam (map (const "") args) default'
match (arg : args) ((PInt i : ps, body) : paths) default' ctx =
  if' (eq arg (Int i)) (match args [(ps, body)] default' ctx) (match (arg : args) paths default' ctx)
match (arg : args) paths default' ctx =
  case' arg (filterMap (pathToCase ctx) paths) (match args (matchAny arg paths) default' ctx) ctx

substitute :: Variable -> Expr -> Expr -> Expr
substitute x a (Var x') | x == x' = a
substitute x a (App b c) = App (substitute x a b) (substitute x a c)
substitute x a (Lam y b) | x /= y = Lam y (substitute x a b)
substitute _ _ b = b

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _ [] = []
filterMap f (x : xs) = case f x of
  Just y -> y : filterMap f xs
  Nothing -> filterMap f xs

-- == Parser == --
parse :: String -> Either Error Expr
parse text = case P.parse text parseExpr of
  Left err -> Left (SyntaxError err)
  Right ast -> Right ast

parseExpr :: Parser Expr
parseExpr =
  P.expression
    [ P.term Var parseVariable,
      P.term Int P.integer,
      P.term (uncurry lam) parseLambda,
      P.term Op2 parseBinaryOperator
    ]
    [ P.infixL 1 (const add) (P.char '+'),
      P.infixL 1 (const sub) (P.char '-'),
      P.infixL 2 (const mul) (P.char '*'),
      P.infixL 3 (const App) P.spaces
    ]

parseVariable :: Parser String
parseVariable = do
  c <- P.oneOf [P.letter, P.char '_']
  cs <- P.zeroOrMore (P.oneOf [P.alphanumeric, P.char '_'])
  P.succeed (c : cs)

parseLambda :: Parser ([String], Expr)
parseLambda = do
  _ <- P.char '\\'
  xs <- P.zeroOrMore (do _ <- P.spaces; parseVariable)
  _ <- P.spaces
  _ <- P.text "->"
  _ <- P.spaces
  a <- parseExpr
  P.succeed (xs, a)

parseBinaryOperator :: Parser BinaryOperator
parseBinaryOperator = do
  _ <- P.char '('
  _ <- P.spaces
  op <-
    P.oneOf
      [ fmap (const Add) (P.char '+'),
        fmap (const Sub) (P.char '-'),
        fmap (const Mul) (P.char '*')
      ]
  _ <- P.spaces
  _ <- P.char ')'
  P.succeed op
