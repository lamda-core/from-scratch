module Core where

import Data.List (delete, union)
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

data Pattern
  = PAny
  | PInt Int
  | PCtr Constructor [(Pattern, Variable)]
  deriving (Eq, Show)

type Context = [(Constructor, [(Constructor, Int)])]

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

defineConstructors :: [(Constructor, Int)] -> Context -> Context
defineConstructors alts ctx = map (\(ctr, _) -> (ctr, alts)) alts ++ ctx

app :: Term -> [Term] -> Term
app = foldl App

lam :: [Variable] -> Term -> Term
lam xs a = foldr Lam a xs

let' :: (String, Term) -> Term -> Term
let' (x, a) b = App (Lam x b) a

add :: Term -> Term -> Term
add a b = app (Op2 Add) [a, b]

sub :: Term -> Term -> Term
sub a b = app (Op2 Sub) [a, b]

mul :: Term -> Term -> Term
mul a b = app (Op2 Mul) [a, b]

eq :: Term -> Term -> Term
eq a b = app (Op2 Eq) [a, b]

if' :: Term -> Term -> Term -> Term
if' cond then' else' = app cond [then', else']

match :: [([(Pattern, Variable)], Term)] -> Context -> Term
match [] _ = Err
match (([], a) : _) _ = a
match paths ctx = do
  let findAlts :: [([(Pattern, Variable)], Term)] -> Maybe [(Constructor, Int)]
      -- TODO: Maybe refactor and merge with `match []` case
      findAlts [] = Nothing
      findAlts (((PCtr ctr _, _) : _, _) : _) = lookup ctr ctx
      findAlts (_ : paths) = findAlts paths

  let matchAny :: Variable -> ([(Pattern, Variable)], Term) -> Maybe ([(Pattern, Variable)], Term)
      matchAny x ((PAny, y) : ps, a) = Just (ps, let' (y, Var x) a)
      matchAny _ _ = Nothing

  let matchCtr :: Variable -> (Constructor, Int) -> ([(Pattern, Variable)], Term) -> Maybe ([(Pattern, Variable)], Term)
      matchCtr x (_, n) ((PAny, y) : ps, a) = Just (replicate n (PAny, "") ++ ps, let' (y, Var x) a)
      matchCtr x (ctr, _) ((PCtr ctr' qs, y) : ps, a) | ctr == ctr' = Just (qs ++ ps, let' (y, Var x) a)
      matchCtr _ _ _ = Nothing

  let freeVars = map snd paths |> map freeVariables |> foldl union []
  let x = newName freeVars "%"
  let other = match (filterMap (matchAny x) paths) ctx
  case findAlts paths of
    Just alts -> do
      let cases =
            map (matchCtr x) alts
              |> map (`filterMap` paths)
              |> map (`match` ctx)
      Lam x (app (Var x) cases)
    Nothing -> Lam x other

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

-- substitute :: Variable -> Term -> Term -> Term
-- substitute x a (Var x') | x == x' = a
-- substitute x a (App b c) = App (substitute x a b) (substitute x a c)
-- substitute x a (Lam y b) | x /= y = Lam y (substitute x a b)
-- substitute _ _ b = b

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _ [] = []
filterMap f (x : xs) = case f x of
  Just y -> y : filterMap f xs
  Nothing -> filterMap f xs
