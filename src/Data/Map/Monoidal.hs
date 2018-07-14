module Data.Map.Monoidal where

import qualified Data.Map as Map

newtype Map k v = Map { unMap :: Map.Map k v }
