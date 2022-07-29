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
        p <- token pattern
        succeed (p, x),
      do
        p <- token pattern
        succeed (p, ""),
      do
        x <- token variableName
        succeed (PAny, x)
    ]

pattern :: Parser Pattern
pattern = do
  oneOf
    [ fmap (const PAny) (char '_'),
      fmap PInt integer,
      do
        ctr <- token constructorName
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
  expr <- token term
  succeed (bindings, const expr)

term :: Parser Term
term = do
  let lambda :: Parser Term
      lambda = do
        _ <- token (char '\\')
        xs <- oneOrMore (token variableName)
        _ <- token (char '.')
        a <- term
        succeed (foldr Lam a xs)

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
    [ atom (const Err) (char '_'),
      atom Var variableName,
      atom Int integer,
      atom id lambda,
      atom Op2 binop,
      prefix (const id) comment,
      inbetween (const id) (char '(') (char ')')
    ]
    [ infixL 1 (\_ a b -> App (App (Op2 Eq) a) b) (text "=="),
      infixL 2 (\_ a b -> App (App (Op2 Add) a) b) (char '+'),
      infixL 2 (\_ a b -> App (App (Op2 Sub) a) b) (char '-'),
      infixL 3 (\_ a b -> App (App (Op2 Mul) a) b) (char '*'),
      infixL 4 (const App) spaces
    ]

typeAlternative :: Parser (Constructor, Int)
typeAlternative = do
  name <- constructorName
  arity <- integer
  succeed (name, arity)

typeDefinition :: Parser (Context -> Context)
typeDefinition = do
  name <- token typeName
  let args = [] -- TODO
  alts <- oneOrMore typeAlternative
  succeed (defineType name args alts)

context :: Parser (Context -> Context)
context = do
  defs <- zeroOrMore typeDefinition
  succeed (\ctx -> foldr id ctx defs)

definition :: Parser (String, Expr)
definition = do
  name <- token variableName
  _ <- token (char '=')
  term' <- token term
  succeed (name, const term')

expression :: Parser Expr
expression = do
  defs <- zeroOrMore definition
  term' <- token term
  succeed (let' defs (const term'))
