import ParserTests (parserTests)
import Test.Hspec
import TypedTests (typedTests)

main :: IO ()
main = hspec $ do
  parserTests
  typedTests
