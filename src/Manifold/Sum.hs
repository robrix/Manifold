{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, TypeOperators #-}
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


instance (Foldable t, Foldable (Sum ts)) => Foldable (Sum (t ': ts)) where
  foldMap f (Here t) = foldMap f t
  foldMap f (There ts) = foldMap f ts

instance (Functor t, Functor (Sum ts)) => Functor (Sum (t ': ts)) where
  fmap f (Here t) = Here (fmap f t)
  fmap f (There ts) = There (fmap f ts)
