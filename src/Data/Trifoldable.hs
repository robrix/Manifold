module Data.Trifoldable where

class Trifoldable t where
  trifoldMap :: Monoid m => (a -> m) -> (b -> m) -> (c -> m) -> t a b c -> m
