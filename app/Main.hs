module Main where

import Reducer.NameSubstitution (evaluate)
import qualified System.Environment
import Tao

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    (filename : _) -> do
      src <- readFile filename
      case fromText src of
        Right term -> print (evaluate term)
        Left err -> putStrLn ("❌ " ++ show err)
    _ -> putStrLn "🛑 Please give me a file to run."
