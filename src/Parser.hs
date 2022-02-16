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

assert :: Bool -> String -> Parser ()
assert check message = if check then succeed () else expected message

orElse :: Parser a -> Parser a -> Parser a
orElse (Parser else') (Parser p) = do
  Parser
    ( \state ->
        case p state of
          Left _ -> else' state
          x -> x
    )

oneOf :: [Parser a] -> Parser a
oneOf [] = expected ""
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

charIf :: (Char -> Bool) -> String -> Parser Char
charIf condition message = do
  ch <- anyChar
  _ <- assert (condition ch) message
  succeed ch

space :: Parser Char
space = charIf Char.isSpace "a blank space"

letter :: Parser Char
letter = charIf Char.isLetter "a letter"

lower :: Parser Char
lower = charIf Char.isLower "a lowercase letter"

upper :: Parser Char
upper = charIf Char.isUpper "an uppercase letter"

digit :: Parser Char
digit = charIf Char.isDigit "a digit from 0 to 9"

alphanumeric :: Parser Char
alphanumeric = charIf Char.isAlphaNum "a letter or digit"

punctuation :: Parser Char
punctuation = charIf Char.isPunctuation "a punctuation character"

char :: Char -> Parser Char
char c = charIf (\ch -> Char.toLower c == Char.toLower ch) ("the character '" <> [c] <> "'")

charCaseSensitive :: Char -> Parser Char
charCaseSensitive c = charIf (== c) ("the character '" <> [c] <> "' (case sensitive)")

-- Sequences
optional :: Parser a -> Parser (Maybe a)
optional parser = fmap Just parser |> orElse (succeed Nothing)

zeroOrOne :: Parser a -> Parser [a]
zeroOrOne parser = fmap (: []) parser |> orElse (succeed [])

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = foldR (:) []

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

-- TODO: add tests
foldL :: (b -> a -> b) -> b -> Parser a -> Parser b
foldL f initial parser =
  do
    x <- parser
    foldL f (f initial x) parser
    |> orElse (succeed initial)

-- TODO: add tests
foldR :: (a -> b -> b) -> b -> Parser a -> Parser b
foldR f final parser =
  do
    x <- parser
    y <- foldR f final parser
    succeed (f x y)
    |> orElse (succeed final)

-- TODO: until
-- TODO: split
-- TODO: splitWithDelimiters

-- Common
token :: Parser a -> Parser a
token parser = do
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
    |> orElse (expected "a number like 123 or 3.14")

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
type Operator a = Int -> (Int -> Parser a) -> Parser (a, Int)

term :: (a -> b) -> Parser a -> Operator b
term f parser prec _ = do
  x <- parser
  succeed (f x, prec)

prefix :: (op -> a -> a) -> Parser op -> Operator a
prefix f op prec expr = do
  op' <- op
  y <- expr prec
  succeed (f op' y, prec)

inbetween :: Parser open -> Parser close -> Operator a
inbetween open close prec expr = do
  _ <- open
  y <- expr 0
  _ <- close
  succeed (y, prec)

infixL :: Int -> (op -> a -> a -> a) -> Parser op -> a -> Operator a
infixL prec f op x lastPrec expr = do
  _ <- assert (lastPrec < prec) ""
  op' <- op
  y <- expr prec
  succeed (f op' x y, lastPrec)

infixR :: Int -> (op -> a -> a -> a) -> Parser op -> a -> Operator a
infixR prec f op x lastPrec expr = do
  _ <- assert (lastPrec <= prec) ""
  op' <- op
  y <- expr prec
  succeed (f op' x y, lastPrec)

expression :: [Int -> (Int -> Parser a) -> Parser (a, Int)] -> [a -> Int -> (Int -> Parser a) -> Parser (a, Int)] -> Parser a
expression unaryOperators binaryOperators =
  let unary rbp expr = oneOf (fmap (\op -> op rbp expr) unaryOperators)
      binary x rbp expr = oneOf (fmap (\op -> op x rbp expr) binaryOperators)
      expr rbp = do
        (x, rbp) <- unary rbp expr
        expr2 x rbp
      expr2 x rbp =
        do
          (y, lbp) <- binary x rbp expr
          expr2 y lbp
          |> orElse (succeed x)
   in expr 0
