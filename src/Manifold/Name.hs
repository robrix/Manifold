module Manifold.Name where

data Name
  = N String
  | I Int
  deriving (Eq, Ord, Show)
