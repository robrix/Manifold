module Manifold.Erasure where

data Erasure
  = Erased
  | Present
  deriving (Eq, Ord, Show)
