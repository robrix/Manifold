{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, LambdaCase #-}
module Manifold.Value.Intro where

import Data.Bifoldable
import Data.Bifunctor
import Data.Foldable (fold)
import Data.Trifoldable
import Data.Trifunctor
import Manifold.Name
import Manifold.Pretty

data Intro var scope recur
  = Bool Bool
  | Abs var scope
  | Pair recur recur
  | Data Name [recur]
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Trifoldable Intro where
  trifoldMap f g h = \case
    Bool _    -> mempty
    Abs v b   -> f v <> g b
    Pair a b  -> h a <> h b
    Data _ as -> foldMap h as

instance Trifunctor Intro where
  trimap f g h = \case
    Bool b    -> Bool b
    Abs v b   -> f v `Abs` g b
    Pair a b  -> h a `Pair` h b
    Data c as -> Data c (map h as)

instance Bifoldable (Intro var) where
  bifoldMap = trifoldMap (const mempty)

instance Bifunctor (Intro var) where
  bimap = trimap id

instance (Pretty var, Pretty scope, Pretty recur) => Pretty (Intro var scope recur) where
  prettyPrec d = \case
    Bool b -> prettyString (show b)
    Abs v b -> prettyParen (d > 0) $ backslash <+> prettyPrec 0 v <+> dot <+> prettyPrec 0 b
    Pair a b -> prettyParen (d > (-1)) $ prettyPrec (-1) a <> comma <+> prettyPrec 0 b
    Data (N "Unit") [] -> parens mempty
    Data c as -> prettyParen (d > 10) $ prettyPrec 10 c <> fold (map ((space <>) . prettyPrec 11) as)
