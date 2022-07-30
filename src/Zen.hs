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

variableName :: Parser String
variableName = do
  -- TODO: support `-` and other characters, maybe URL-like names
  c <- lowercase
  cs <- zeroOrMore (oneOf [alphanumeric, char '_'])
  succeed (c : cs)

constructorName :: Parser String
constructorName = do
  -- TODO: support `-` and other characters, maybe URL-like names, or keep types CamelCase?
  c <- uppercase
  cs <- zeroOrMore (oneOf [alphanumeric, char '_'])
  succeed (c : cs)

typeName :: Parser String
typeName = constructorName

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
        x <- token variableName
        _ <- token (char '@')
        p <- pattern
        succeed (p, x),
      do
        p <- pattern
        succeed (p, ""),
      do
        x <- variableName
        succeed (PAny, x)
    ]

pattern :: Parser Pattern
pattern = do
  oneOf
    [ fmap (const PAny) (char '_'),
      fmap PInt integer,
      do
        ctr <- constructorName
        ps <- zeroOrMore (do _ <- spaces; binding)
        succeed (PCtr ctr ps),
      do
        _ <- token (char '(')
        p <- token pattern
        _ <- char ')'
        succeed p
    ]

case' :: Parser Case
case' = do
  _ <- token (char '|')
  bindings <- oneOrMore (token binding)
  _ <- token (text "->")
  expr <- expression
  succeed (bindings, expr)

expression :: Parser Expr
expression = do
  let lambda :: Parser Expr
      lambda = do
        _ <- token (char '\\')
        xs <- oneOrMore (token variableName)
        _ <- token (char '.')
        a <- expression
        succeed (lam xs a)

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
        _ <- char ')'
        succeed op

  withOperators
    [ prefix let' (oneOrMore definition),
      atom (const err) (char '_'),
      atom var variableName,
      atom int integer,
      atom id lambda,
      atom (const . Op2) binop,
      atom match (oneOrMore case'),
      prefix (const id) comment,
      inbetween (const id) (char '(') (char ')')
    ]
    [ infixL 1 (const eq) (text "=="),
      infixL 2 (const add) (char '+'),
      infixL 2 (const sub) (char '-'),
      infixL 3 (const mul) (char '*'),
      infixL 4 (\_ a b -> app a [b]) spaces
    ]

typeAlternative :: Parser (Constructor, Int)
typeAlternative = do
  name <- token constructorName
  arity <- integer
  succeed (name, arity)

typeDefinition :: Parser (Context -> Context)
typeDefinition = do
  name <- token typeName
  let args = [] -- TODO
  alts <- oneOrMore (do _ <- spaces; typeAlternative)
  succeed (defineType name args alts)

context :: Parser (Context -> Context)
context = do
  defs <- zeroOrMore typeDefinition
  succeed (\ctx -> foldr id ctx defs)

definition :: Parser (Variable, Expr)
definition = do
  name <- token variableName
  _ <- token (char '=')
  expr <- expression
  -- TODO: support newlines and indentation aware parsing
  _ <- zeroOrMore (oneOf [char ' ', char '\t'])
  _ <- oneOf [char '\n', char ';']
  succeed (name, expr)
