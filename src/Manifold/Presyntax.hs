{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, LambdaCase #-}
module Manifold.Presyntax where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Manifold.Name

data Expr usage recur
  = Unit
  | Bool
  | T
  | F
  | Set
  | Var Name
  | Constraint usage :-> recur
  | Abs (Constraint usage) recur
  | App recur recur
  | If recur recur recur
  | recur :* recur
  | Nth Int recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 0 :->
infixl 7 :*


newtype Type usage = Type { unType :: Expr usage (Type usage) }
  deriving (Eq, Ord, Show)

newtype Term usage = Term { unTerm :: Expr usage (Term usage) }
  deriving (Eq, Ord, Show)


data Constraint usage = Binding usage ::: Type usage
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infix 5 :::

type Binding usage = (Name, usage)


instance Bifoldable Expr where
  bifoldMap f g = \case
    Unit         -> mempty
    Bool         -> mempty
    T            -> mempty
    F            -> mempty
    Set          -> mempty
    Var _        -> mempty
    var :-> body -> foldMap f var <> g body
    Abs var body -> foldMap f var <> g body
    App f a      -> g f <> g a
    If c t e     -> g c <> g t <> g e
    a :* b       -> g a <> g b
    Nth _ a      -> g a

instance Bifunctor Expr where
  bimap f g = \case
    Unit         -> Unit
    Bool         -> Bool
    T            -> T
    F            -> F
    Set          -> Set
    Var name     -> Var name
    var :-> body -> fmap f var :-> g body
    Abs var body -> Abs (fmap f var) (g body)
    App f a      -> App (g f) (g a)
    If c t e     -> If (g c) (g t) (g e)
    a :* b       -> g a :* g b
    Nth i a      -> Nth i (g a)

instance Bitraversable Expr where
  bitraverse f g = \case
    Unit         -> pure Unit
    Bool         -> pure Bool
    T            -> pure T
    F            -> pure F
    Set          -> pure Set
    Var name     -> pure (Var name)
    var :-> body -> (:->) <$> traverse f var <*> g body
    Abs var body -> Abs <$> traverse f var <*> g body
    App f a      -> App <$> g f <*> g a
    If c t e     -> If <$> g c <*> g t <*> g e
    a :* b       -> (:*) <$> g a <*> g b
    Nth i a      -> Nth i <$> g a


instance Foldable Type where
  foldMap f = bifoldMap f (foldMap f) . unType

instance Functor Type where
  fmap f = Type . bimap f (fmap f) . unType

instance Traversable Type where
  traverse f = fmap Type . bitraverse f (traverse f) . unType


instance Foldable Term where
  foldMap f = bifoldMap f (foldMap f) . unTerm

instance Functor Term where
  fmap f = Term . bimap f (fmap f) . unTerm

instance Traversable Term where
  traverse f = fmap Term . bitraverse f (traverse f) . unTerm
