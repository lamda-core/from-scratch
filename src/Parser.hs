module Parser where

import qualified Data.Char as Char

type Parser a = State -> Either Error (a, State)

data State = State
  { source :: String,
    remaining :: String,
    lastChar :: Maybe Char,
    token :: Token,
    stack :: [Token]
  }
  deriving (Eq, Show)

data Token = Token
  { name :: String,
    row :: Int,
    col :: Int
  }
  deriving (Eq, Show)

data Error = Expected String State
  deriving (Eq, Show)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

infixl 1 |>

parse :: String -> Parser a -> Either Error a
parse source parser = do
  let initialState =
        State
          { source = source,
            remaining = source,
            lastChar = Nothing,
            token = Token {name = "", row = 1, col = 1},
            stack = []
          }
  fmap fst (parser initialState)

succeed :: a -> Parser a
succeed value state = Right (value, state)

expected :: String -> Parser a
expected message state = Left (Expected message state)

expecting :: String -> Parser a -> Parser a
expecting message parser state = case parser state of
  Left (Expected _ state) -> Left (Expected message state)
  x -> x

andThen :: (a -> Parser b) -> Parser a -> Parser b
andThen f parser state = do
  (x, state) <- parser state
  f x state

-- Single characters
anyChar :: Parser Char
anyChar state@State {remaining = ch : remaining, token = tok} =
  state
    { remaining = remaining,
      lastChar = Just ch,
      token =
        if ch == '\n'
          then tok {row = row tok + 1, col = 1}
          else tok {col = col tok + 1}
    }
    |> succeed ch
anyChar state = expected "a character" state

charIf :: (Char -> Bool) -> String -> Parser Char
charIf condition errorMessage =
  andThen
    (\ch -> if condition ch then succeed ch else expected errorMessage)
    anyChar

space :: Parser Char
space = charIf Char.isSpace "a blank space"

letter :: Parser Char
letter = charIf Char.isLetter "a letter"

letterLower :: Parser Char
letterLower = charIf Char.isLower "a lowercase letter"

letterUpper :: Parser Char
letterUpper = charIf Char.isUpper "an uppercase letter"

digit :: Parser Char
digit = charIf Char.isDigit "a digit from 0 to 9"

alphanumeric :: Parser Char
alphanumeric = charIf Char.isAlphaNum "a letter or digit"

punctuation :: Parser Char
punctuation = charIf Char.isPunctuation "a punctuation character"

char :: Char -> Parser Char
char ch = charIf (\c -> Char.toLower ch == Char.toLower c) ("the character '" <> [ch] <> "'")

charCaseSensitive :: Char -> Parser Char
charCaseSensitive ch = charIf (ch ==) ("the character '" <> [ch] <> "' (case sensitive)")

except :: Parser Char -> Parser Char
except parser state =
  case parser state of
    Right (_, state) -> expected "something else" state
    Left _ -> anyChar state

-- Sequences
zeroOrOne :: Parser a -> Parser [a]
zeroOrOne parser state = case parser state of
  Right (x, state) -> succeed [x] state
  Left _ -> succeed [] state

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore parser state = case parser state of
  Right (x, state) -> case zeroOrMore parser state of
    Right (xs, state) -> succeed (x : xs) state
    Left _ -> succeed [x] state
  Left _ -> succeed [] state

oneOrMore :: Parser a -> Parser [a]
oneOrMore parser state = do
  (x, state) <- parser state
  (xs, state) <- zeroOrMore parser state
  succeed (x : xs) state

chain :: [Parser a] -> Parser [a]
chain [] state = Right ([], state)
chain (parser : parsers) state = do
  (x, state) <- parser state
  (xs, state) <- chain parsers state
  succeed (x : xs) state

-- Common
-- text
-- textNoCase