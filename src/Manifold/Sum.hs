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

instance Foldable (Sum '[]) where
  foldMap _ u = case u of {}

instance (Functor t, Functor (Sum ts)) => Functor (Sum (t ': ts)) where
  fmap f = either (inject . fmap f) (weaken . fmap f) . decompose

instance Functor (Sum '[]) where
  fmap _ u = case u of {}

instance (Traversable t, Traversable (Sum ts)) => Traversable (Sum (t ': ts)) where
  traverse f = either (fmap inject . traverse f) (fmap weaken . traverse f) . decompose

instance Traversable (Sum '[]) where
  traverse _ u = case u of {}

instance (Show (t a), Show (Sum ts a)) => Show (Sum (t ': ts) a) where
  showsPrec d = either (showsPrec d) (showsPrec d) . decompose

instance Show (Sum '[] a) where
  showsPrec _ u = case u of {}

instance (Eq (t a), Eq (Sum ts a)) => Eq (Sum (t ': ts) a) where
  Here t1 == Here t2 = t1 == t2
  There ts1 == There ts2 = ts1 == ts2
  _ == _ = False

instance Eq (Sum '[] a) where
  u1 == _ = case u1 of {}

instance (Ord (t a), Ord (Sum ts a)) => Ord (Sum (t ': ts) a) where
  compare (Here t1) (Here t2) = compare t1 t2
  compare (There ts1) (There ts2) = compare ts1 ts2
  compare (Here _) (There _) = LT
  compare _ _ = GT

instance Ord (Sum '[] a) where
  compare u1 = case u1 of {}
