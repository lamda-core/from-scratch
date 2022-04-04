import CoreTests (coreTests)
import ParserTests (parserTests)
import PatternMatchingTests (patternMatchingTests)
import Test.Hspec

main :: IO ()
main = hspec $ do
  parserTests
  coreTests
  patternMatchingTests
