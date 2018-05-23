module Manifold.Pretty where

class Pretty a where
  prettyPrec :: Int -> a -> ShowS

pretty :: Pretty a => a -> String
pretty = ($ "") . prettyPrec 0
