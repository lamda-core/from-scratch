module Main where

-- import Core (parse)
-- import qualified System.Environment

main :: IO ()
main = do
  -- args <- System.Environment.getArgs
  -- case args of
  --   (filename : _) -> do
  --     contents <- readFile filename
  --     case parse contents of
  --       Right expr -> print expr
  --       Left err -> putStrLn ("❌ " ++ show err)
  --   _ -> putStrLn "🛑 Please give me a file to run."
  putStrLn "Hello"
