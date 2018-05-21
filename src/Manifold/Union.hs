{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}
module Manifold.Union where

data Union ts a where
  Here :: t a -> Union (t ': ts) a
  There :: Union ts a -> Union (t ': ts) a
