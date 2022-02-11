module Parser where

import qualified Data.Char as Char

newtype Parser a = Parser (State -> Either Error (a, State))

data State = State
  { source :: String,
    remaining :: String,
    lastChar :: Maybe Char,
    current :: Token,
    stack :: [Token]
  }
  deriving (Eq, Show)

data Token = Token
  { name :: String,
    row :: Int,
    col :: Int
  }
  deriving (Eq, Show)

data Error = Error String State
  deriving (Eq, Show)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

infixl 1 |>

instance Functor Parser where
  fmap f (Parser p) =
    Parser
      ( \state -> do
          (x, state) <- p state
          Right (f x, state)
      )

instance Applicative Parser where
  pure = succeed
  parserF <*> parser = do
    f <- parserF
    fmap f parser

instance Monad Parser where
  Parser p >>= f =
    Parser
      ( \state -> do
          (x, state) <- p state
          let (Parser p') = f x
          (y, state) <- p' state
          Right (y, state)
      )
  return x = succeed x

parse :: String -> Parser a -> Either Error a
parse source (Parser p) = do
  let initialState =
        State
          { source = source,
            remaining = source,
            lastChar = Nothing,
            current = Token {name = "", row = 1, col = 1},
            stack = []
          }
  fmap fst (p initialState)

succeed :: a -> Parser a
succeed value = Parser (\state -> Right (value, state))

expected :: String -> Parser a
expected message = Parser (Left . Error message)

orElse :: Parser a -> Parser a -> Parser a
orElse (Parser else') (Parser p) = do
  Parser
    ( \state ->
        case p state of
          Left _ -> else' state
          x -> x
    )

oneOf :: [Parser a] -> Parser a
oneOf [] = expected "something"
oneOf (p : ps) = p |> orElse (oneOf ps)

-- Single characters

anyChar :: Parser Char
anyChar =
  let advance state@State {remaining = ch : remaining, current = tok} =
        Right
          ( ch,
            state
              { remaining = remaining,
                lastChar = Just ch,
                current =
                  if ch == '\n'
                    then tok {row = row tok + 1, col = 1}
                    else tok {col = col tok + 1}
              }
          )
      advance state = Left (Error "a character" state)
   in Parser advance

space :: Parser Char
space = do
  ch <- anyChar
  if Char.isSpace ch then succeed ch else expected "a blank space"

letter :: Parser Char
letter = do
  ch <- anyChar
  if Char.isLetter ch then succeed ch else expected "a letter"

lower :: Parser Char
lower = do
  ch <- anyChar
  if Char.isLower ch then succeed ch else expected "a lowercase letter"

upper :: Parser Char
upper = do
  ch <- anyChar
  if Char.isUpper ch then succeed ch else expected "an uppercase letter"

digit :: Parser Char
digit = do
  ch <- anyChar
  if Char.isDigit ch then succeed ch else expected "a digit from 0 to 9"

alphanumeric :: Parser Char
alphanumeric = do
  ch <- anyChar
  if Char.isAlphaNum ch then succeed ch else expected "a letter or digit"

punctuation :: Parser Char
punctuation = do
  ch <- anyChar
  if Char.isPunctuation ch then succeed ch else expected "a punctuation character"

char :: Char -> Parser Char
char c = do
  ch <- anyChar
  if Char.toLower c == Char.toLower ch then succeed ch else expected $ "the character '" <> [c] <> "'"

charCaseSensitive :: Char -> Parser Char
charCaseSensitive c = do
  ch <- anyChar
  if c == ch then succeed ch else expected $ "the character '" <> [c] <> "' (case sensitive)"

-- Sequences
optional :: Parser a -> Parser (Maybe a)
optional parser = fmap Just parser |> orElse (succeed Nothing)

zeroOrOne :: Parser a -> Parser [a]
zeroOrOne parser = fmap (: []) parser |> orElse (succeed [])

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore parser =
  do
    x <- parser
    xs <- zeroOrMore parser
    succeed (x : xs)
    |> orElse (succeed [])

oneOrMore :: Parser a -> Parser [a]
oneOrMore parser = do
  x <- parser
  xs <- zeroOrMore parser
  succeed (x : xs)

chain :: [Parser a] -> Parser [a]
chain [] = succeed []
chain (p : ps) = do
  x <- p
  xs <- chain ps
  succeed (x : xs)

exactly :: Int -> Parser a -> Parser [a]
exactly n parser = chain (replicate n parser)

atLeast :: Int -> Parser a -> Parser [a]
atLeast min parser | min <= 0 = zeroOrMore parser
atLeast min parser = do
  x <- parser
  xs <- atLeast (min - 1) parser
  succeed (x : xs)

atMost :: Int -> Parser a -> Parser [a]
atMost max _ | max <= 0 = succeed []
atMost max parser =
  do
    x <- parser
    xs <- atMost (max - 1) parser
    succeed (x : xs)
    |> orElse (succeed [])

between :: Int -> Int -> Parser a -> Parser [a]
between min max parser | min <= 0 = atMost max parser
between min max parser = do
  x <- parser
  xs <- between (min - 1) (max - 1) parser
  succeed (x : xs)

-- TODO: until
-- TODO: split
-- TODO: splitWithDelimiters

-- Common
tok :: Parser a -> Parser a
tok parser = do
  x <- parser
  _ <- zeroOrMore space
  succeed x

integer :: Parser Int
integer =
  do
    digits <- oneOrMore digit
    succeed (read digits)
    |> orElse (expected "an integer value like 123")

number :: Parser Float
number =
  do
    int <- oneOrMore digit
    oneOf
      [ do
          _ <- char '.'
          fraction <- oneOrMore digit
          succeed (read $ concat [int, ['.'], fraction]),
        do succeed (read int)
      ]
    |> orElse (expected "a fractional number like 3.14")

text :: String -> Parser String
text str =
  chain (fmap char str)
    |> orElse (expected $ "the text '" <> str <> "'")

textCaseSensitive :: String -> Parser String
textCaseSensitive str =
  chain (fmap charCaseSensitive str)
    |> orElse (expected $ "the text '" <> str <> "' (case sensitive)")

-- TODO: line
-- TODO: date
-- TODO: time
-- TODO: datetime
-- TODO: email
-- TODO: unixPath
-- TODO: windowsPath
-- TODO: uri
-- TODO: IPv4
-- TODO: IPv6
-- PROGRAMMING LANGUAGES
-- TODO: identifier
-- TODO: intBin
-- TODO: intOct
-- TODO: intHex
-- TODO: intExp
-- TODO: numberExp
-- TODO: quotedText
-- TODO: collection

-- Operator precedence
expression :: Parser (Int, a) -> Parser (Int, a -> a) -> Parser a
expression prefix infix' = do
  (lbp, lhs) <- prefix
  (rbp, rhs) <- infix'
  if rbp < lbp
    then expression (succeed (rbp, rhs lhs)) infix'
    else succeed lhs

-- unaryExpr :: Parser (Int, a -> a -> a) -> Parser (Int, a -> a) -> Parser a -> Parser (Int, a)
-- unaryExpr binary unary term =
--   do
--     (rbp, op) <- unary
--     rhs <- expr binary unary term
--     succeed (rbp, op rhs)
--     |> orElse (fmap (\x -> (0, x)) term)

-- binaryExpr :: Parser (Int, a -> a -> a) -> Parser (Int, a -> a) -> Parser a -> Parser (Int, a)
-- binaryExpr binary unary term = do
--   (rbp, lhs) <- unaryExpr

-- expr :: Parser (Int, a -> a -> a) -> Parser (Int, a -> a) -> Parser a -> Parser a
-- expr binary unary term = do
--   (rbp, lhs) <- unaryExpr unary term
--   do
--     rhs <- expr binary unary term
--     succeed lhs
--     |> orElse (succeed lhs)