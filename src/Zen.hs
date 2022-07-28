module Zen where

import Core
import Parser

-- TODO: AST type for pretty printing

newtype Error
  = SyntaxError ParserError
  deriving (Eq, Show)

-- parse :: String -> Either Error Term
-- parse text = case Parser.parse text parseTerm of
--   Left err -> Left (SyntaxError err)
--   Right term -> Right term

variable :: Parser String
variable = do
  -- TODO: support `-` and other characters, maybe URL-like names
  c <- lowercase
  cs <- zeroOrMore (oneOf [alphanumeric, char '_'])
  succeed (c : cs)

constructor :: Parser String
constructor = do
  -- TODO: support `-` and other characters, maybe URL-like names, or keep types CamelCase?
  c <- uppercase
  cs <- zeroOrMore (oneOf [alphanumeric, char '_'])
  succeed (c : cs)

binaryOperator :: Parser BinaryOperator
binaryOperator =
  oneOf
    [ fmap (const Add) (char '+'),
      fmap (const Sub) (char '-'),
      fmap (const Mul) (char '*'),
      fmap (const Eq) (text "==")
    ]

comment :: Parser String
comment = do
  _ <- text "--"
  _ <- zeroOrOne space
  until' (== '\n') anyChar

binding :: Parser Binding
binding = do
  oneOf
    [ do
        x <- token variable
        _ <- token (char '@')
        p <- token pattern
        succeed (p, x),
      do
        p <- token pattern
        succeed (p, ""),
      do
        x <- token variable
        succeed (PAny, x)
    ]

pattern :: Parser Pattern
pattern = do
  oneOf
    [ fmap (const PAny) (char '_'),
      fmap PInt integer,
      do
        ctr <- token constructor
        ps <- zeroOrMore (token binding)
        succeed (PCtr ctr ps),
      do
        _ <- token (char '(')
        p <- token pattern
        _ <- token (char ')')
        succeed p
    ]

case' :: Parser Case
case' = do
  _ <- token (char '|')
  bindings <- oneOrMore (token binding)
  _ <- token (text "->")
  expr <- token expression
  succeed (bindings, expr)

expression :: Parser Term
expression = do
  let lambda :: Parser ([Variable], Term)
      lambda = do
        _ <- token (char '\\')
        xs <- oneOrMore (token variable)
        _ <- token (char '.')
        a <- expression
        succeed (xs, a)

  let binop :: Parser BinaryOperator
      binop = do
        let operators =
              [ fmap (const Add) (char '+'),
                fmap (const Sub) (char '-'),
                fmap (const Mul) (char '*'),
                fmap (const Eq) (text "==")
              ]
        _ <- token (char '(')
        op <- token (oneOf operators)
        _ <- token (char ')')
        succeed op

  withOperators
    [ term (const Err) (char '_'),
      term Var variable,
      term Int integer,
      term (uncurry lam) lambda,
      term Op2 binop,
      prefix (const id) comment,
      inbetween (const id) (char '(') (char ')')
    ]
    [ infixL 1 (const eq) (text "=="),
      infixL 2 (const add) (char '+'),
      infixL 2 (const sub) (char '-'),
      infixL 3 (const mul) (char '*'),
      infixL 4 (const App) spaces
    ]
