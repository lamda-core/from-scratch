import CoreTests (coreTests)
import ParserTests (parserTests)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  parserTests
  coreTests
