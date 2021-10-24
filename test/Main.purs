module Test.Main where

import Prelude

import Effect (Effect)
import Test.PatternTests (patternTests)
import Test.TypedTests (typedTests)
import Test.Unit.Main (runTest)
import Test.UntypedTests (untypedTests)

main :: Effect Unit
main = runTest do
    untypedTests
    typedTests
    patternTests
