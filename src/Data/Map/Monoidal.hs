module Data.Map.Monoidal where

import qualified Data.Map as Map

newtype Map k v = Map { unMap :: Map.Map k v }

insert :: (Ord k, Semigroup v) => k -> v -> Map k v -> Map k v
insert k v = Map . Map.insertWith (<>) k v . unMap
