{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, MultiParamTypeClasses, TypeOperators #-}
module Manifold.Sum
( Sum
, Element(..)
, decompose
) where

data Sum ts a where
  Here :: t a -> Sum (t ': ts) a
  There :: Sum ts a -> Sum (t ': ts) a


class Element t ts where
  inject :: t a -> Sum ts a
  project :: Sum ts a -> Maybe (t a)

instance {-# OVERLAPPABLE #-} Element t (t ': ts) where
  inject = Here
  project (Here t) = Just t
  project _        = Nothing

instance {-# OVERLAPPABLE #-} Element t ts => Element t (t' ': ts) where
  inject = There . inject
  project (There t) = project t
  project _         = Nothing


decompose :: Sum (t ': ts) a -> Either (t a) (Sum ts a)
decompose (Here t) = Left t
decompose (There ts) = Right ts
