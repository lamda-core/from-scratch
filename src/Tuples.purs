module Tuples where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data T2 a b = T2 a b
derive instance (Eq a, Eq b) => Eq (T2 a b)
derive instance Generic (T2 a b) _
instance (Show a, Show b) => Show (T2 a b) where
  show = genericShow

first :: forall a b. T2 a b -> a
first (T2 a _) = a

second :: forall a b. T2 a b -> b
second (T2 _ b) = b

data T3 a b c = T3 a b c
derive instance (Eq a, Eq b, Eq c) => Eq (T3 a b c)
derive instance Generic (T3 a b c) _
instance (Show a, Show b, Show c) => Show (T3 a b c) where
  show = genericShow
