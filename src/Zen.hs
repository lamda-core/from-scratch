module Zen where

import Core (Constructor, Expr, Variable)
import qualified Core as C
import Parser (Parser)
import qualified Parser as P

data AST
  = Err
  | Var Variable
  | Int Int
  | App AST AST
  | Lam Variable AST
  | If AST AST AST
  | Case AST [(Constructor, AST)] AST
  | Match [Path] AST
  | Add
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

type Path = ([Pattern], AST)

data Context = Context
  { definitions :: [(String, AST)],
    constructors :: [(Constructor, ([Variable], [Constructor]))]
  }

newtype Error
  = SyntaxError P.Error
  deriving (Eq, Show)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

infixl 1 |>

app :: AST -> [AST] -> AST
app = foldl App

lam :: [Variable] -> AST -> AST
lam xs a = foldr Lam a xs

add :: AST -> AST -> AST
add a b = app Add [a, b]

sub :: AST -> AST -> AST
sub a b = app Sub [a, b]

mul :: AST -> AST -> AST
mul a b = app Mul [a, b]

eq :: AST -> AST -> AST
eq a b = app Eq [a, b]

empty :: Context
empty = Context {definitions = [], constructors = []}

defineType :: String -> [(Constructor, [Variable])] -> Context -> Context
defineType _ ctrs ctx =
  let ctrDefs = map (\(ctr, xs) -> (ctr, (xs, map fst ctrs))) ctrs
   in ctx {constructors = ctrDefs ++ constructors ctx}

define :: String -> AST -> Context -> Context
define name paths ctx = ctx {definitions = (name, paths) : definitions ctx}

constructorAlternatives :: Constructor -> Context -> Maybe [Constructor]
constructorAlternatives ctr ctx = fmap snd (lookup ctr (constructors ctx))

constructorArguments :: Constructor -> Context -> Maybe [Variable]
constructorArguments ctr ctx = fmap fst (lookup ctr (constructors ctx))

toCore :: AST -> Context -> Expr
toCore Err _ = C.Err
toCore (Var x) _ = C.Var x
toCore (Int i) _ = C.Int i
toCore (App a b) ctx = C.App (toCore a ctx) (toCore b ctx)
toCore (Lam x a) ctx = C.Lam x (toCore a ctx)
toCore (If cond then' else') ctx = toCore (app cond [then', else']) ctx
toCore (Case _ [] c) ctx = toCore c ctx
toCore (Case a cases@((ctr, _) : _) c) ctx = do
  let findCase :: Constructor -> AST
      findCase ctr = case find (\(ctr', _) -> ctr == ctr') cases of
        Just (_, b) -> b
        Nothing -> c

  -- TODO: Case should examine the input's type to get the constructor alternatives
  case constructorAlternatives ctr ctx of
    Just alts -> toCore (app a (map findCase alts)) ctx
    Nothing -> toCore c ctx
toCore (Match [] default') ctx = toCore default' ctx
toCore (Match (([], a) : _) _) ctx = toCore a ctx
toCore (Match ((PInt i : ps, a) : paths) default') ctx = do
  let x = "_" ++ show i
  let cond = If (eq (Var x) (Int i)) a (App (Match paths default') (Var x))
  toCore (Match ((PVar x : ps, cond) : paths) default') ctx
toCore (Match paths default') ctx = do
  let inferName :: [Path] -> Variable
      inferName [] = ""
      inferName ((PVar x : _, _) : _) = x
      inferName (_ : paths) = inferName paths
  let x = inferName paths

  let getCtr :: Context -> Path -> Maybe (Constructor, [Variable])
      getCtr ctx (PCtr ctr _ : _, _) = do
        args <- constructorArguments ctr ctx
        Just (ctr, args)
      getCtr _ _ = Nothing
  let ctrs = unique (filterMap (getCtr ctx) paths)

  let rename :: [Variable] -> [Pattern] -> AST -> AST
      rename (x : xs) (PVar y : ps) a | x /= y = rename xs ps (App (Lam y a) (Var x))
      rename (_ : xs) (_ : ps) a = rename xs ps a
      rename _ _ a = a

  let matchAny :: Variable -> Path -> Maybe Path
      matchAny _ (PAny : ps, a) = Just (ps, a)
      matchAny x (PVar y : ps, a) = Just (ps, rename [x] [PVar y] a)
      matchAny _ _ = Nothing
  let others = filterMap (matchAny x) paths

  let matchCtr :: Variable -> Constructor -> [Variable] -> Path -> Maybe Path
      matchCtr _ _ args (PAny : ps, a) = Just (map PVar args ++ ps, a)
      matchCtr x _ args (PVar y : ps, a) = Just (map PVar args ++ ps, rename [x] [PVar y] a)
      matchCtr _ ctr args (PCtr ctr' qs : ps, a) | ctr == ctr' = Just (map PVar args ++ ps, rename args qs a)
      matchCtr _ _ _ _ = Nothing
  let caseOf :: Variable -> [Path] -> AST -> (Constructor, [Variable]) -> (Constructor, AST)
      caseOf x paths default' (ctr, args) =
        (ctr, Match (filterMap (matchCtr x ctr args) paths) default')
  let case' = Lam x (Case (Var x) (map (caseOf x paths default') ctrs) (Match others default'))
  toCore case' ctx
toCore Add _ = C.Op2 C.Add
toCore Sub _ = C.Op2 C.Sub
toCore Mul _ = C.Op2 C.Mul
toCore Eq _ = C.Op2 C.Eq

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x : _) | f x = Just x
find f (_ : xs) = find f xs

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _ [] = []
filterMap f (x : xs) = case f x of
  Just y -> y : filterMap f xs
  Nothing -> filterMap f xs

-- == Parser == --
parse :: String -> Either Error AST
parse text = case P.parse text parseExpr of
  Left err -> Left (SyntaxError err)
  Right ast -> Right ast

-- parseModule :: Parser Module
-- parseModule =
--   P.oneOf
--     [ do
--         (name, value) <- parseLetBinding
--         module' <- parseModule
--         define
--     ]

parseLetBinding :: Parser (String, AST)
parseLetBinding = do
  _ <- P.char '@'
  _ <- P.spaces
  name <- parseVariable
  _ <- P.spaces
  _ <- P.char '='
  _ <- P.spaces
  expr <- parseExpr
  P.succeed (name, expr)

parseDefinitions :: Parser [(String, AST)]
parseDefinitions = P.zeroOrMore (P.oneOf [parseLetBinding])

parseExpr :: Parser AST
parseExpr =
  P.expression
    [ P.term Var parseVariable,
      P.term Int P.integer,
      P.term (uncurry lam) parseLambda,
      P.term id parseBinaryOperator,
      P.prefix (const id) parseComment
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

parseLambda :: Parser ([String], AST)
parseLambda = do
  _ <- P.char '\\'
  xs <- P.zeroOrMore (do _ <- P.spaces; parseVariable)
  _ <- P.spaces
  _ <- P.text "->"
  _ <- P.spaces
  a <- parseExpr
  P.succeed (xs, a)

parseBinaryOperator :: Parser AST
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

parseComment :: Parser String
parseComment = do
  _ <- P.text "--"
  comment <- P.until' (== '\n') P.anyChar
  P.succeed comment