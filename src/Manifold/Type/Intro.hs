{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, LambdaCase #-}
module Manifold.Type.Intro where

import Data.Bifoldable
import Data.Bifunctor
import Data.Foldable (fold)
import Data.Trifoldable
import Data.Trifunctor
import Manifold.Name
import Manifold.Pretty

data IntroT var scope recur
  = TypeT
  | TypeC Name [recur]
  | var :-> scope
  | recur :* recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 0 :->
infixl 7 :*


instance Trifoldable IntroT where
  trifoldMap f g h = \case
    TypeT      -> mempty
    TypeC _ as -> foldMap h as
    v :-> b    -> f v <> g b
    a :* b     -> h a <> h b

instance Trifunctor IntroT where
  trimap f g h = \case
    TypeT      -> TypeT
    TypeC c as -> TypeC c (map h as)
    v :-> b    -> f v :-> g b
    a :* b     -> h a :* h b

instance Bifoldable (IntroT var) where
  bifoldMap = trifoldMap (const mempty)

instance Bifunctor (IntroT var) where
  bimap = trimap id

instance (Pretty var, Pretty scope, Pretty recur) => Pretty (IntroT var scope recur) where
  prettyPrec d = \case
    TypeT -> prettyString "Type"
    TypeC c as -> prettyParen (d > 10) $ prettyPrec 10 c <> fold (map ((space <>) . prettyPrec 11) as)
    v :-> b -> prettyParen (d > 0) $ prettyPrec 1 v <+> prettyString "->" <+> prettyPrec 0 b
    a :* b -> prettyParen (d > 7) $ prettyPrec 7 a <+> prettyString "*" <+> prettyPrec 8 b
