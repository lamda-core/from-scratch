module Result where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Prelude (class Eq, class Show)

data Result err a
  = Ok a
  | Err err

derive instance eqResult :: (Eq err, Eq a) => Eq (Result err a)
derive instance genericResult :: Generic (Result err a) _
instance showResult :: (Show err, Show a) => Show (Result err a) where
  show x = genericShow x
