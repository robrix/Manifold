{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}
module Manifold.Sum
( Sum
) where

data Sum ts a where
  Here :: t a -> Sum (t ': ts) a
  There :: Sum ts a -> Sum (t ': ts) a
