import Core
import Criterion.Main
import Reducer.NameSubstitution (evaluate)
import Tao

nativeFctorial :: Int -> Int
nativeFctorial 0 = 1
nativeFctorial n = n * nativeFctorial (n - 1)

interpret :: String -> IO Term
interpret filename = do
  src <- readFile filename
  case fromText src of
    Right term -> return term
    Left err -> fail (show err)

main :: IO ()
main = do
  factorial <- fmap const (interpret "bench/src/factorial.tao")
  defaultMain
    [ bgroup
        "native factorial"
        [ bench "0" $ whnf nativeFctorial 0,
          bench "10" $ whnf nativeFctorial 10
        ],
      bgroup
        "factorial"
        [ bench "0" $ whnf evaluate (app factorial [int 0] empty),
          bench "10" $ whnf evaluate (app factorial [int 10] empty)
          -- bench "10" $ whnfIO (factorial 10)
        ]
    ]
