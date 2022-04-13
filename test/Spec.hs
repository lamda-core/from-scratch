import CoreTests (coreTests)
import ParserTests (parserTests)
import Test.Hspec

main :: IO ()
main = hspec $ do
  parserTests
  coreTests
