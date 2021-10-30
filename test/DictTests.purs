module Test.DictTests where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Dict (Dict, KV(..), dict, empty, get, has, set, union)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

dictTests :: Free TestF Unit
dictTests = 
  suite "--== Dict ==--" do
    test "☯︎ empty" do
      empty # Assert.equal (dict [] :: Dict Int Int)

    test "☯︎ has" do
      has "x" empty # Assert.equal false
      has "x" (dict [KV "x" 1]) # Assert.equal true
      has "x" (dict [KV "y" 1]) # Assert.equal false

    test "☯︎ get" do
      get "x" empty # Assert.equal (Nothing :: Maybe Int)
      get "x" (dict [KV "x" 1]) # Assert.equal (Just 1)
      get "x" (dict [KV "y" 2]) # Assert.equal Nothing

    test "☯︎ set" do
      set "x" 1 empty # Assert.equal (dict [KV "x" 1])
      set "x" 1 (dict [KV "x" 2]) # Assert.equal (dict [KV "x" 1])
      set "x" 1 (dict [KV "y" 2]) # Assert.equal (dict [KV "y" 2, KV "x" 1])

    test "☯︎ union" do
      empty `union` (dict [KV "x" 1]) # Assert.equal (dict [KV "x" 1])
      (dict [KV "x" 1]) `union` (dict [KV "x" 2]) # Assert.equal (dict [KV "x" 2])
      (dict [KV "x" 1]) `union` (dict [KV "y" 2]) # Assert.equal (dict [KV "x" 1, KV "y" 2])
