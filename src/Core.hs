module Core where

import Data.List (union)
import Text.Read (readMaybe)

-- Bidirectional type checking: https://youtu.be/utyBNDj7s2w
-- https://www.cse.iitk.ac.in/users/ppk/teaching/cs653/notes/lectures/Lambda-calculus.lhs.pdf

type Variable = String

type Constructor = String

data Term
  = Err
  | Var Variable
  | Int Int
  | App Term Term
  | Lam Variable Term
  | Op2 BinaryOperator
  deriving (Eq)

data BinaryOperator
  = Add
  | Sub
  | Mul
  | Eq
  deriving (Eq)

type Type = Term

data Pattern
  = PAny
  | PInt Int
  | PCtr Constructor [Binding]
  deriving (Eq, Show)

type Binding = (Pattern, Variable)

type Case = ([Binding], Expr)

-- TODO: make Context opaque
type Context = [(Constructor, [(Constructor, Int)])]

type Expr = Context -> Term

instance Show Term where
  show Err = "_"
  show (Var x) = x
  show (Int i) = show i
  show (App (Lam x b) a) = x ++ " = " ++ show a ++ "; " ++ show b
  show (App a b) = case b of
    App _ _ -> show a ++ " (" ++ show b ++ ")"
    Lam _ _ -> show a ++ " (" ++ show b ++ ")"
    _ -> show a ++ " " ++ show b
  show (Lam x a) = do
    let vars :: Term -> [Variable] -> ([Variable], Term)
        vars (Lam x a) xs = let (xs', a') = vars a xs in (x : xs', a')
        vars a xs = (xs, a)
    let (xs, a') = vars a []
    "\\" ++ unwords (x : xs) ++ ". " ++ show a'
  show (Op2 op) = "(" ++ show op ++ ")"

instance Show BinaryOperator where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Eq = "=="

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

infixl 1 |>

-- Context
empty :: Context
empty = []

defineType :: String -> [Variable] -> [(Constructor, Int)] -> Context -> Context
defineType _ _ alts ctx = map (\(ctr, _) -> (ctr, alts)) alts ++ ctx

-- High level constructs
err :: Expr
err = const Err

var :: Variable -> Expr
var x = const (Var x)

int :: Int -> Expr
int i = const (Int i)

app :: Expr -> [Expr] -> Expr
app a bs ctx = foldl App (a ctx) (map (\b -> b ctx) bs)

lam :: [Variable] -> Expr -> Expr
lam xs a ctx = foldr Lam (a ctx) xs

add :: Expr -> Expr -> Expr
add a b = app (const (Op2 Add)) [a, b]

sub :: Expr -> Expr -> Expr
sub a b = app (const (Op2 Sub)) [a, b]

mul :: Expr -> Expr -> Expr
mul a b = app (const (Op2 Mul)) [a, b]

eq :: Expr -> Expr -> Expr
eq a b = app (const (Op2 Eq)) [a, b]

if' :: Expr -> Expr -> Expr -> Expr
if' cond then' else' = app cond [then', else']

let' :: [(Variable, Expr)] -> Expr -> Expr
let' defs a ctx = case a ctx of
  Var x -> case lookup x defs of
    Just b -> let' defs b ctx
    Nothing -> Var x
  App a b -> App (let' defs (const a) ctx) (let' defs (const b) ctx)
  Lam x a -> Lam x (let' (filter (\(y, _) -> x /= y) defs) (const a) ctx)
  a -> a

match :: [Case] -> Expr
match [] = err
match (([], a) : _) = a
match (((PInt i, x) : ps, a) : cases) = lam [x] (if' (eq (var x) (int i)) (match [(ps, a)]) (match cases))
match cases = \ctx -> do
  let findAlts :: [Case] -> Maybe [(Constructor, Int)]
      findAlts [] = Nothing
      findAlts (((PCtr ctr _, _) : _, _) : _) = lookup ctr ctx
      findAlts (_ : cases) = findAlts cases

  let matchAny :: Variable -> Case -> Maybe Case
      matchAny x ((PAny, y) : ps, a) = Just (ps, let' [(y, var x)] a)
      matchAny _ _ = Nothing

  let matchCtr :: Variable -> (Constructor, Int) -> Case -> Maybe Case
      matchCtr x (_, n) ((PAny, y) : ps, a) = Just (replicate n (PAny, "") ++ ps, let' [(y, var x)] a)
      matchCtr x (ctr, _) ((PCtr ctr' qs, y) : ps, a) | ctr == ctr' = Just (qs ++ ps, let' [(y, var x)] a)
      matchCtr _ _ _ = Nothing

  let freeVars = map snd cases |> map (\a -> freeVariables (a ctx)) |> foldl union []
  let x = newName freeVars "%"
  case findAlts cases of
    Just alts -> do
      let branches =
            map (matchCtr x) alts
              |> map (`filterMap` cases)
              |> map match
      lam [x] (app (var x) branches) ctx
    Nothing -> lam [x] (match (filterMap (matchAny x) cases)) ctx

-- Helper functions
freeVariables :: Term -> [String]
freeVariables (Var x) = [x]
freeVariables (App a b) = freeVariables a `union` freeVariables b
freeVariables (Lam x a) = delete x (freeVariables a)
freeVariables _ = []

newName :: [String] -> String -> String
newName used x = case lastNameIndex x used of
  Just i -> x ++ show (i + 1)
  Nothing -> x ++ "0"

nameIndex :: String -> String -> Maybe Int
nameIndex "" x = readMaybe x
nameIndex (ch : prefix) (ch' : x) | ch == ch' = nameIndex prefix x
nameIndex _ _ = Nothing

lastNameIndex :: String -> [String] -> Maybe Int
lastNameIndex _ [] = Nothing
lastNameIndex prefix (x : xs) = case lastNameIndex prefix xs of
  Just i -> case nameIndex prefix x of
    Just j -> Just (max i j)
    Nothing -> Just i
  Nothing -> if prefix == x then Just 0 else nameIndex prefix x

-- Standard library functions
filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _ [] = []
filterMap f (x : xs) = case f x of
  Just y -> y : filterMap f xs
  Nothing -> filterMap f xs

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (x' : xs) | x == x' = delete x xs
delete x (y : xs) = y : delete x xs

-- TODO: union : [a] -> [a] -> [a]
-- TODO: readInt : String -> Maybe Int
