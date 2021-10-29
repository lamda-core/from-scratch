module Test.Main where

import Prelude

import Effect (Effect)
import Test.DictTests (dictTests)
import Test.TypedTests (typedTests)
import Test.Unit.Main (runTest)
import Test.UntypedTests (untypedTests)

main :: Effect Unit
main = runTest do
    dictTests
    untypedTests
    typedTests
