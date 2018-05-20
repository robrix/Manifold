{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, LambdaCase #-}
module Manifold.Presyntax where

import Data.Bifoldable
import Manifold.Name

data Constraint usage = Binding usage ::: Type usage
  deriving (Eq, Foldable, Ord, Show)

infix 5 :::

type Binding usage = (Name, usage)


data Expr usage recur
  = Bool
  | T
  | F
  | Set
  | Constraint usage :-> recur
  | Var Name
  | Abs (Constraint usage) recur
  | App recur recur
  | If recur recur recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Bifoldable Expr where
  bifoldMap f g = \case
    Bool         -> mempty
    T            -> mempty
    F            -> mempty
    Set          -> mempty
    var :-> body -> foldMap f var <> g body
    Var _        -> mempty
    Abs var body -> foldMap f var <> g body
    App f a      -> g f <> g a
    If c t e     -> g c <> g t <> g e


newtype Type usage = Type { unType :: Expr usage (Type usage) }
  deriving (Eq, Ord, Show)

instance Foldable Type where
  foldMap f = bifoldMap f (foldMap f) . unType

newtype Term usage = Term { unTerm :: Expr usage (Term usage) }
  deriving (Eq, Ord, Show)
