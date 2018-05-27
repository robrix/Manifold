module Data.Trifunctor where

class Trifunctor t where
  trimap :: (a -> a') -> (b -> b') -> (c -> c') -> t a b c -> t a' b' c'
