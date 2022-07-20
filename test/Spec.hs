import CoreTests (coreTests)
import ParserTests (parserTests)
import Test.Hspec (hspec)
import ZenTests (zenTests)

main :: IO ()
main = hspec $ do
  parserTests
  coreTests
  zenTests
