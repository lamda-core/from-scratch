module Dict where

import Prelude

import Data.Array (foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

data KV k v = KV k v

derive instance (Eq k, Eq v) => Eq (KV k v)
derive instance Generic (KV k v) _
instance (Show k, Show v) => Show (KV k v) where
  show x = genericShow x

type Dict k v = List (KV k v)

empty :: forall k v. Eq k => Dict k v
empty = Nil

dict :: forall k v. Eq k => Array (KV k v) -> Dict k v
dict = foldl (\kvs (KV k v) -> set k v kvs) empty

has :: forall k v. Eq k => k -> Dict k v -> Boolean
has _ Nil = false
has k (KV k' _ : _) | k == k' = true
has k (_ : kvs) = has k kvs

get :: forall k v. Eq k => k -> Dict k v -> Maybe v
get _ Nil = Nothing
get k (KV k' v : _) | k == k' = Just v
get k (_ : kvs) = get k kvs

set :: forall k v. Eq k => k -> v -> List (KV k v) -> List (KV k v)
set k v Nil = KV k v : Nil
set k v (KV k' _ : kvs) | k == k' = KV k v : kvs
set k v (KV k' v' : kvs) = KV k' v' : set k v kvs

union :: forall k v. Eq k => Dict k v -> Dict k v -> Dict k v
union Nil kvs2 = kvs2
union (KV k _ : kvs1) kvs2 | has k kvs2 = kvs1 `union` kvs2
union (KV k v : kvs1) kvs2 = KV k v : kvs1 `union` kvs2