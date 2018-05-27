{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, LambdaCase #-}
module Manifold.Value.Intro where

import Data.Trifoldable
import Data.Trifunctor
import Data.Bifoldable
import Data.Bifunctor
import Manifold.Pretty

data Intro var scope recur
  = Unit
  | Bool Bool
  | Abs var scope
  | Pair recur recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Trifoldable Intro where
  trifoldMap f g h = \case
    Unit     -> mempty
    Bool _   -> mempty
    Abs v b  -> f v <> g b
    Pair a b -> h a <> h b

instance Trifunctor Intro where
  trimap f g h = \case
    Unit     -> Unit
    Bool b   -> Bool b
    Abs v b  -> f v `Abs` g b
    Pair a b -> h a `Pair` h b

instance Bifoldable (Intro var) where
  bifoldMap = trifoldMap (const mempty)

instance Bifunctor (Intro var) where
  bimap = trimap id

instance (Pretty var, Pretty scope, Pretty recur) => Pretty (Intro var scope recur) where
  prettyPrec d = \case
    Unit -> showParen True id
    Bool b -> showsPrec d b
    Abs v b -> showParen (d > 0) $ showChar '\\' . showChar ' ' . prettyPrec 0 v . showChar ' '  . showChar '.' . showChar ' ' . prettyPrec 0 b
    Pair a b -> showParen (d > (-1)) $ prettyPrec (-1) a . showChar ',' . showChar ' ' . prettyPrec 0 b
