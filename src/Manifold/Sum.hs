{-# LANGUAGE DataKinds, GADTs, MultiParamTypeClasses, TypeOperators #-}
module Manifold.Sum
( Sum
, Element(..)
) where

data Sum ts a where
  Here :: t a -> Sum (t ': ts) a
  There :: Sum ts a -> Sum (t ': ts) a

class Element t ts where
  inject :: t a -> Sum ts a
  project :: Sum ts a -> Maybe (t a)
