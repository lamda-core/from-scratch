module Result where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Prelude (class Apply, class Bind, class Eq, class Functor, class Show)

data Result err a
  = Ok a
  | Err err

derive instance (Eq err, Eq a) => Eq (Result err a)
derive instance Generic (Result err a) _
instance (Show err, Show a) => Show (Result err a) where
  show x = genericShow x
derive instance Functor (Result err)
instance Apply (Result err) where
  apply (Ok f) (Ok a) = Ok (f a)
  apply (Err err) _ = Err err
  apply _ (Err err) = Err err
instance Bind (Result err) where
  bind (Ok a) f = f a
  bind (Err err) _ = Err err
