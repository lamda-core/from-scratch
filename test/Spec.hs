import CoreTests (coreTests)
import ParserTests (parserTests)
import qualified Reducer.NameSubstitution as NameSubstitution
import ReducerTests (reducerTests)
import TaoTests (taoTests)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  parserTests
  coreTests
  taoTests
  reducerTests "Name substitution" NameSubstitution.evaluate
