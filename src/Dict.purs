module Dict where

import Prelude

import Data.Array (find, findIndex, findMap, snoc, unionBy, updateAt)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.String (joinWith)

data KV k v = KV k v

derive instance (Eq k, Eq v) => Eq (KV k v)
derive instance Generic (KV k v) _
instance (Show k, Show v) => Show (KV k v) where
  show (KV k v) = show k <> ": " <> show v

newtype Dict k v = Dict (Array (KV k v))

derive instance (Eq k, Eq v) => Eq (Dict k v)
instance (Show k, Show v) => Show (Dict k v) where
  show (Dict kvs) = "{" <> joinWith ", " (map show kvs) <> "}"

empty :: forall k v. Eq k => Dict k v
empty = Dict []

has :: forall k v. Eq k => k -> Dict k v -> Boolean
has k (Dict kvs) = isJust (find (\(KV k' _) -> k == k') kvs)

get :: forall k v. Eq k => k -> Dict k v -> Maybe v
get k (Dict kvs) = findMap (\(KV k' v) -> if k == k' then Just v else Nothing) kvs

set :: forall k v. Eq k => k -> v -> Dict k v -> Dict k v
set k v (Dict kvs) = case findIndex (\(KV k' _) -> k == k') kvs of
  Just i -> case updateAt i (KV k v) kvs of
    Just kvs' -> Dict kvs'
    Nothing -> Dict kvs
  Nothing -> Dict (snoc kvs (KV k v))

union :: forall k v. Eq k => Dict k v -> Dict k v -> Dict k v
union (Dict kvs1) (Dict kvs2) = Dict (unionBy (\(KV k1 _) (KV k2 _) -> k1 == k2) kvs2 kvs1)