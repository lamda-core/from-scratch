import Core (Term (..))
import Reducer.NameSubstitution (evaluate)
import qualified System.Environment
import Tao

evaluateFile :: String -> IO Term
evaluateFile filename = do
  src <- readFile filename
  case fromText src of
    Right term -> return (evaluate term)
    Left err -> return Err --("❌ " ++ show err)

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    -- TODO: support passing arguments to expression.
    (filename : _) -> do
      term <- evaluateFile filename
      print term
    _ -> putStrLn "🛑 Please give me a file to run."
