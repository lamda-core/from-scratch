module Zen where

import Core
import Parser hiding (parse)
import qualified Parser

newtype Error
  = SyntaxError ParserError
  deriving (Eq, Show)

parse :: String -> Either Error Term
parse text = case Parser.parse text parseTerm of
  Left err -> Left (SyntaxError err)
  Right term -> Right term

-- parseModule :: Parser Module
-- parseModule =
--   oneOf
--     [ do
--         (name, value) <- parseLetBinding
--         module' <- parseModule
--         define
--     ]

parseLetBinding :: Parser (String, Term)
parseLetBinding = do
  _ <- char '@'
  _ <- spaces
  name <- parseVariable
  _ <- spaces
  _ <- char '='
  _ <- spaces
  expr <- parseTerm
  succeed (name, expr)

parseDefinitions :: Parser [(String, Term)]
parseDefinitions = zeroOrMore (oneOf [parseLetBinding])

parseTerm :: Parser Term
parseTerm =
  expression
    [ term Var parseVariable,
      term Int integer,
      -- term (uncurry lam) parseLambda,
      term id parseBinaryOperator,
      prefix (const id) parseComment
    ]
    [ infixL 1 (const add) (char '+'),
      infixL 1 (const sub) (char '-'),
      infixL 2 (const mul) (char '*'),
      infixL 3 (const App) spaces
    ]

parseVariable :: Parser String
parseVariable = do
  c <- oneOf [letter, char '_']
  cs <- zeroOrMore (oneOf [alphanumeric, char '_'])
  succeed (c : cs)

parseLambda :: Parser ([String], Term)
parseLambda = do
  _ <- char '\\'
  xs <- zeroOrMore (do _ <- spaces; parseVariable)
  _ <- spaces
  _ <- text "->"
  _ <- spaces
  a <- parseTerm
  succeed (xs, a)

parseBinaryOperator :: Parser Term
parseBinaryOperator = do
  _ <- char '('
  _ <- spaces
  op <-
    oneOf
      [ fmap (const Add) (char '+'),
        fmap (const Sub) (char '-'),
        fmap (const Mul) (char '*')
      ]
  _ <- spaces
  _ <- char ')'
  succeed (Op2 op)

parseComment :: Parser String
parseComment = do
  _ <- text "--"
  comment <- until' (== '\n') anyChar
  succeed comment