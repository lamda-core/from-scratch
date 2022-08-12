import Criterion.Main

nativeFctorial :: Int -> Int
nativeFctorial 0 = 1
nativeFctorial n = n * nativeFctorial (n - 1)

factorial :: Int -> IO ()
factorial n = do
  f <- evaluateFile "bench/src/factorial.tao"
  print ""

main :: IO ()
main =
  defaultMain
    [ bgroup
        "native factorial"
        [ bench "0" $ whnf nativeFctorial 0,
          bench "10" $ whnf nativeFctorial 10
          --   ],
          -- bgroup
          --   "factorial"
          --   [ bench "0" $ whnfIO (factorial 0),
          --     bench "10" $ whnfIO (factorial 10)
        ]
    ]
