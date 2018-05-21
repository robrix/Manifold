{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Manifold.Constraint where

import Data.Bifoldable
import Data.Bifunctor
import Manifold.Name

data Constraint usage recur = (Name, usage) ::: recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Bifoldable Constraint where
  bifoldMap f g ((_, usage) ::: ty) = f usage <> g ty

instance Bifunctor Constraint where
  bimap f g ((name, usage) ::: ty) = (name, f usage) ::: g ty

infix 5 :::

constraintName :: Constraint usage recur -> Name
constraintName (var ::: _) = name var

constraintValue :: Constraint usage recur -> recur
constraintValue (_ ::: ty) = ty
