module Test.DictTests where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Dict (Dict(..), KV(..), empty, get, has, set, union)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

dictTests :: Free TestF Unit
dictTests = 
  suite "--== Dict ==--" do
    test "☯︎ empty" do
      empty # Assert.equal (Dict [] :: Dict Int Int)

    test "☯︎ has" do
      has "x" empty # Assert.equal false
      has "x" (Dict [KV "x" 1]) # Assert.equal true
      has "x" (Dict [KV "y" 1]) # Assert.equal false

    test "☯︎ get" do
      get "x" empty # Assert.equal (Nothing :: Maybe Int)
      get "x" (Dict [KV "x" 1]) # Assert.equal (Just 1)
      get "x" (Dict [KV "y" 2]) # Assert.equal Nothing

    test "☯︎ set" do
      set "x" 1 empty # Assert.equal (Dict [KV "x" 1])
      set "x" 1 (Dict [KV "x" 2]) # Assert.equal (Dict [KV "x" 1])
      set "x" 1 (Dict [KV "y" 2]) # Assert.equal (Dict [KV "y" 2, KV "x" 1])

    test "☯︎ union" do
      empty `union` (Dict [KV "x" 1]) # Assert.equal (Dict [KV "x" 1])
      (Dict [KV "x" 1]) `union` (Dict [KV "x" 2]) # Assert.equal (Dict [KV "x" 2])
      (Dict [KV "x" 1]) `union` (Dict [KV "y" 2]) # Assert.equal (Dict [KV "y" 2, KV "x" 1])
