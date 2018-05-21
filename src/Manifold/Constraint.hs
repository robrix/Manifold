{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Manifold.Constraint where

import Data.Bifoldable
import Data.Bifunctor
import Manifold.Name

data Constraint var recur = var ::: recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infix 5 :::

instance Bifoldable Constraint where
  bifoldMap f g (var ::: ty) = f var <> g ty

instance Bifunctor Constraint where
  bimap f g (var ::: ty) = f var ::: g ty


constraintName :: Named var => Constraint var recur -> Name
constraintName (var ::: _) = name var

constraintVar :: Constraint var recur -> var
constraintVar (var ::: _) = var

constraintValue :: Constraint var recur -> recur
constraintValue (_ ::: ty) = ty
