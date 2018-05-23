{-# LANGUAGE DefaultSignatures #-}
module Manifold.Pretty where

class Pretty a where
  prettyPrec :: Int -> a -> ShowS
  default prettyPrec :: Show a => Int -> a -> ShowS
  prettyPrec = showsPrec

prettys :: Pretty a => a -> ShowS
prettys = prettyPrec 0

pretty :: Pretty a => a -> String
pretty = ($ "") . prettys


instance Pretty ()
