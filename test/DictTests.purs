module Test.DictTests where

import Prelude

import Control.Monad.Free (Free)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Dict (Dict, KV(..), dict, empty, get, has, set, union)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

dictTests :: Free TestF Unit
dictTests = 
  suite "--== Dict ==--" do
    test "☯︎ empty" do
      empty # Assert.equal (Nil :: Dict Int Int)

    test "☯︎ dict" do
      dict [] # Assert.equal (Nil :: Dict Int Int)
      dict [KV "x" 1] # Assert.equal (KV "x" 1 : Nil)
      dict [KV "x" 1, KV "x" 2] # Assert.equal (KV "x" 2 : Nil)
      dict [KV "x" 1, KV "y" 2] # Assert.equal (KV "x" 1 : KV "y" 2 : Nil)

    test "☯︎ has" do
      has "x" empty # Assert.equal false
      has "x" (dict [KV "x" 1]) # Assert.equal true
      has "x" (dict [KV "y" 1]) # Assert.equal false

    test "☯︎ get" do
      get "x" Nil # Assert.equal (Nothing :: Maybe Int)
      get "x" (dict [KV "x" 1]) # Assert.equal (Just 1)
      get "x" (dict [KV "y" 2]) # Assert.equal Nothing

    test "☯︎ set" do
      set "x" 1 Nil # Assert.equal (KV "x" 1 : Nil)
      set "x" 1 (dict [KV "x" 2]) # Assert.equal (dict [KV "x" 1])
      set "x" 1 (dict [KV "y" 2]) # Assert.equal (dict [KV "y" 2, KV "x" 1])

    test "☯︎ union" do
      Nil `union` (dict [KV "x" 1]) # Assert.equal (dict [KV "x" 1])
      (dict [KV "x" 1]) `union` (dict [KV "x" 2]) # Assert.equal (dict [KV "x" 2])
      (dict [KV "x" 1]) `union` (dict [KV "y" 2]) # Assert.equal (dict [KV "x" 1, KV "y" 2])
