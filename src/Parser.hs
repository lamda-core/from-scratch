module Parser where

type Parser a = State -> Either Error (a, State)

data State = State
  { text :: String,
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
parse text parser = do
  (x, _) <-
    parser
      ( State
          { text = text,
            remaining = text,
            lastChar = Nothing,
            token = Token {name = "", row = 1, col = 1},
            stack = []
          }
      )
  Right x

succeed :: a -> Parser a
succeed value state = Right (value, state)

expected :: String -> Parser a
expected message state = Left (Expected message state)

expecting :: String -> Parser a -> Parser a
expecting message parser state =
  case parser state of
    Left (Expected _ state) -> Left (Expected message state)
    x -> x

andThen :: (a -> Parser b) -> Parser a -> Parser b
andThen f parser state = do
  (x, s) <- parser state
  f x s

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

char :: Char -> Parser Char
char ch =
  anyChar
    |> andThen (\c -> if c == ch then succeed c else expected "")
    |> expecting ("the character '" <> [ch] <> "'")