{-# LANGUAGE DataKinds, EmptyCase, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, TypeOperators #-}
module Manifold.Sum
( Sum
, Element(..)
, decompose
, strengthen
, weaken
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

strengthen :: Sum '[t] a -> t a
strengthen (Here t) = t
strengthen (There ts) = case ts of {}

weaken :: Sum ts a -> Sum (t ': ts) a
weaken = There


instance (Foldable t, Foldable (Sum ts)) => Foldable (Sum (t ': ts)) where
  foldMap f = either (foldMap f) (foldMap f) . decompose

instance (Functor t, Functor (Sum ts)) => Functor (Sum (t ': ts)) where
  fmap f = either (inject . fmap f) (weaken . fmap f) . decompose

instance (Traversable t, Traversable (Sum ts)) => Traversable (Sum (t ': ts)) where
  traverse f = either (fmap inject . traverse f) (fmap weaken . traverse f) . decompose

instance (Show (t a), Show (Sum ts a)) => Show (Sum (t ': ts) a) where
  showsPrec d = either (showsPrec d) (showsPrec d) . decompose

instance (Eq (t a), Eq (Sum ts a)) => Eq (Sum (t ': ts) a) where
  Here t1 == Here t2 = t1 == t2
  There ts1 == There ts2 = ts1 == ts2
  _ == _ = False
