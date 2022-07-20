module Core where

import Data.List (delete, union)
import Text.Read (readMaybe)

-- Bidirectional type checking: https://youtu.be/utyBNDj7s2w
-- https://www.cse.iitk.ac.in/users/ppk/teaching/cs653/notes/lectures/Lambda-calculus.lhs.pdf

type Variable = String

type Constructor = String

data Expr
  = Err
  | Var Variable
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

nameIndex :: String -> String -> Maybe Int
nameIndex "" x = readMaybe x
nameIndex (ch : prefix) (ch' : x) | ch == ch' = nameIndex prefix x
nameIndex _ _ = Nothing

findLastNameIndex :: String -> [String] -> Maybe Int
findLastNameIndex _ [] = Nothing
findLastNameIndex prefix (x : xs) = case findLastNameIndex prefix xs of
  Just i -> case nameIndex prefix x of
    Just j -> Just (max i j)
    Nothing -> Just i
  Nothing -> if prefix == x then Just 0 else nameIndex prefix x

substitute :: Variable -> Expr -> Expr -> Expr
substitute x a (Var x') | x == x' = a
substitute x a (App b c) = App (substitute x a b) (substitute x a c)
substitute x a (Lam y b) | x /= y = Lam y (substitute x a b)
substitute _ _ b = b

maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap _ [] = []
maybeMap f (x : xs) = case f x of
  Just y -> y : maybeMap f xs
  Nothing -> maybeMap f xs
