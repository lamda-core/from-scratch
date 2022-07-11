module Core where

import Data.List (find)

-- Bidirectional type checking: https://youtu.be/utyBNDj7s2w

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

type Context = Constructor -> Maybe [Constructor]

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

case' :: Expr -> [(Constructor, [Variable], Expr)] -> Expr -> Context -> Expr
case' a cases default' ctx = do
  -- TODO: usie types to list constructors -- listCtrs :: Expr -> Context -> Maybe [Constructor]
  let listCtrs :: [(Constructor, [Variable], Expr)] -> Context -> Maybe [Constructor]
      listCtrs [] _ = Nothing
      listCtrs ((ctr, _, _) : _) ctx = ctx ctr

      findCtr :: Constructor -> [(Constructor, [Variable], Expr)] -> Expr -> Expr
      findCtr c cases default' = case find (\(c', _, _) -> c == c') cases of
        Just (_, xs, a) -> lam xs a
        Nothing -> default'

  case listCtrs cases ctx of
    Just ctrs -> app a (map (\c -> findCtr c cases default') ctrs)
    Nothing -> default'

substitute :: Variable -> Expr -> Expr -> Expr
substitute x a (Var x') | x == x' = a
substitute x a (App b c) = App (substitute x a b) (substitute x a c)
substitute x a (Lam y b) | x /= y = Lam y (substitute x a b)
substitute _ _ b = b

match :: [Expr] -> [([Pattern], Expr)] -> Expr -> Context -> Expr
match [] ((_, body) : _) _ _ = body
match args [] default' _ = lam (map (const "") args) default'
match (arg : args) ((PInt i : ps, body) : paths) default' ctx =
  if' (eq arg (Int i)) (match args [(ps, body)] default' ctx) (match (arg : args) paths default' ctx)
match (arg : args) paths default' ctx = do
  let listCtrs :: [([Pattern], Expr)] -> [(Constructor, Int)]
      listCtrs [] = []
      listCtrs ((PCtr c qs : _, _) : paths) = case listCtrs paths of
        ctrs | c `elem` map fst ctrs -> ctrs
        ctrs -> (c, length qs) : ctrs
      listCtrs (_ : paths) = listCtrs paths

      rename :: [Variable] -> ([Pattern], Expr) -> ([Pattern], Expr)
      rename [] (ps, a) = (ps, a)
      rename xs ([], a) = (map PVar xs, a)
      rename (x : xs) (PVar y : ps, a) = let (ps', a') = rename xs (ps, a) in (PVar x : ps', substitute y (Var x) a')
      rename (_ : xs) (p : ps, a) = let (ps', a') = rename xs (ps, a) in (p : ps', a')

      rename' :: [Variable] -> [Pattern] -> Expr -> Expr
      rename' [] _ a = a
      rename' _ [] a = a
      rename' (x : xs) (PVar y : ps) a | x /= y = rename' xs ps (substitute y (Var x) a)
      rename' (_ : xs) (_ : ps) a = rename' xs ps a

      -- TODO: actually use `filter` and compose with simpler functions for `filterAny` and `filterCtr`
      filterAny :: [([Pattern], Expr)] -> [([Pattern], Expr)]
      filterAny [] = []
      filterAny ((PAny : ps, body) : paths) = (ps, body) : filterAny paths
      filterAny ((PVar x : ps, body) : paths) = (ps, substitute x arg body) : filterAny paths
      filterAny (_ : paths) = filterAny paths

      filterCtr :: Constructor -> [Variable] -> [([Pattern], Expr)] -> [([Pattern], Expr)]
      filterCtr _ _ [] = []
      filterCtr c xs ((PAny : ps, body) : paths) = (map PVar xs ++ ps, body) : filterCtr c xs paths
      filterCtr c xs ((PVar x : ps, body) : paths) = (map PVar xs ++ ps, substitute x arg body) : filterCtr c xs paths
      filterCtr c xs ((PCtr c' qs : ps, body) : paths) | c == c' = rename xs (qs ++ ps, body) : filterCtr c xs paths
      filterCtr c xs (_ : paths) = filterCtr c xs paths

      toCase :: (Constructor, Int) -> (Constructor, [Variable], Expr)
      toCase (c, n) = do
        let xs = ["%" ++ show i | i <- [1 .. n]] -- TODO: make sure these are unique
        (c, xs, match args (filterCtr c xs paths) default' ctx)

  case' arg (map toCase (listCtrs paths)) (match args (filterAny paths) default' ctx) ctx

-- fix :: Expr -> Expr
-- fix = App Fix