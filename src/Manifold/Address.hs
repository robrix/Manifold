module Manifold.Address where

newtype Precise value = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)
