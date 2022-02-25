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

spaces :: Parser String
spaces = zeroOrMore space

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

foldL :: (b -> a -> b) -> b -> Parser a -> Parser b
foldL f initial parser =
  do
    x <- parser
    foldL f (f initial x) parser
    |> orElse (succeed initial)

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

identifier :: Parser Char -> [Parser Char] -> Parser String
identifier first rest = do
  x <- first
  xs <- zeroOrMore (oneOf rest)
  succeed (x : xs)

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
type UnaryOperator a = (Int -> Parser a) -> Parser a

type BinaryOperator a = Int -> a -> UnaryOperator a

term :: (a -> b) -> Parser a -> UnaryOperator b
term f parser _ = do
  x <- parser
  _ <- spaces
  succeed (f x)

prefix :: (op -> a -> a) -> Parser op -> UnaryOperator a
prefix f op expr = do
  op' <- op
  _ <- spaces
  y <- expr 0
  _ <- spaces
  succeed (f op' y)

prefixList :: (a -> b -> b) -> b -> Parser open -> Parser a -> Parser close -> Parser b
prefixList f initial open parser close = do
  _ <- open
  y <- foldR f initial (do _ <- spaces; parser)
  _ <- spaces
  _ <- close
  _ <- spaces
  succeed y

inbetween :: (open -> a -> a) -> Parser open -> Parser close -> UnaryOperator a
inbetween f open close expr = do
  open' <- open
  _ <- spaces
  y <- expr 0
  _ <- spaces
  _ <- close
  _ <- spaces
  succeed (f open' y)

infixL :: Int -> (op -> a -> a -> a) -> Parser op -> BinaryOperator a
infixL opPrec f op prec x expr = do
  _ <- assert (prec < opPrec) ""
  op' <- op
  _ <- spaces
  y <- expr opPrec
  _ <- spaces
  succeed (f op' x y)

infixR :: Int -> (op -> a -> a -> a) -> Parser op -> BinaryOperator a
infixR opPrec f op prec x expr = do
  _ <- assert (prec <= opPrec) ""
  op' <- op
  _ <- spaces
  y <- expr opPrec
  _ <- spaces
  succeed (f op' x y)

expression :: [UnaryOperator a] -> [BinaryOperator a] -> Parser a
expression unaryOperators binaryOperators =
  let unary f = oneOf (fmap (\op -> op f) unaryOperators)
      binary x f prec = oneOf (fmap (\op -> op x f prec) binaryOperators)
      expr prec = do
        x <- unary expr
        expr2 prec x
      expr2 prec x =
        do
          y <- binary prec x expr
          expr2 prec y
          |> orElse (succeed x)
   in expr 0
