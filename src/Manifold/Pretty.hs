module Manifold.Pretty where

class Pretty a where
  prettyPrec :: Int -> a -> ShowS
