{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Manifold.Constraint where

import Data.Bifoldable
import Data.Bifunctor
import Manifold.Name
import Manifold.Pretty

data Constraint var ty = var ::: ty
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infix 5 :::

instance Bifoldable Constraint where
  bifoldMap f g (var ::: ty) = f var <> g ty

instance Bifunctor Constraint where
  bimap f g (var ::: ty) = f var ::: g ty

instance Named var => Named (Constraint var ty) where
  name (var ::: _) = name var
  setName name (var ::: ty) = setName name var ::: ty

instance (Pretty var, Pretty ty) => Pretty (Constraint var ty) where
  prettyPrec d (var ::: ty) = prettyParen (d > 0) $ prettyPrec 0 var <> space <> colon <> space <> prettyPrec 0 ty


constraintName :: Named var => Constraint var ty -> Name
constraintName (var ::: _) = name var

constraintVar :: Constraint var ty -> var
constraintVar (var ::: _) = var

constraintValue :: Constraint var ty -> ty
constraintValue (_ ::: ty) = ty
