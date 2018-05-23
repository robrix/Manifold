{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, LambdaCase #-}
module Manifold.Expr.Intro where

import Manifold.Pretty

data Intro var scope recur
  = Unit
  | Bool Bool
  | Abs var scope
  | Pair recur recur
  | UnitT
  | BoolT
  | TypeT
  | var :-> scope
  | recur :* recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 0 :->
infixl 7 :*

class Trifoldable t where
  trifoldMap :: Monoid m => (a -> m) -> (b -> m) -> (c -> m) -> t a b c -> m

class Trifunctor t where
  trimap :: (a -> a') -> (b -> b') -> (c -> c') -> t a b c -> t a' b' c'

instance Trifoldable Intro where
  trifoldMap f g h = \case
    Unit     -> mempty
    Bool _   -> mempty
    Abs v b  -> f v <> g b
    Pair a b -> h a <> h b
    UnitT    -> mempty
    BoolT    -> mempty
    TypeT    -> mempty
    v :-> b  -> f v <> g b
    a :* b   -> h a <> h b

instance Trifunctor Intro where
  trimap f g h = \case
    Unit     -> Unit
    Bool b   -> Bool b
    Abs v b  -> f v `Abs` g b
    Pair a b -> h a `Pair` h b
    UnitT    -> UnitT
    BoolT    -> BoolT
    TypeT    -> TypeT
    v :-> b  -> f v :-> g b
    a :* b   -> h a :* h b

instance (Pretty var, Pretty scope, Pretty recur) => Pretty (Intro var scope recur) where
  prettyPrec d = \case
    Unit -> showParen True id
    Bool b -> showsPrec d b
    Abs v b -> showParen (d > 0) $ showChar '\\' . showChar ' ' . prettyPrec 0 v . showChar ' '  . showChar '.' . showChar ' ' . prettyPrec 0 b
    Pair a b -> showParen (d > (-1)) $ prettyPrec (-1) a . showChar ',' . showChar ' ' . prettyPrec 0 b
    UnitT -> showString "Unit"
    BoolT -> showString "Bool"
    TypeT -> showString "Type"
    v :-> b -> showParen (d > 0) $ prettyPrec 1 v . showChar ' ' . showString "->" . showChar ' ' . prettyPrec 0 b
    a :* b -> showParen (d > 7) $ prettyPrec 7 a . showChar ' ' . showChar '*' . showChar ' ' . prettyPrec 8 b
