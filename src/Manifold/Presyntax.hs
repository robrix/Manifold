{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, LambdaCase #-}
module Manifold.Presyntax where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Maybe (fromMaybe)
import Manifold.Name
import Manifold.Substitution

data Expr usage recur
  = Unit
  | UnitType
  | BoolType
  | T
  | F
  | TypeType
  | Var Name
  | Constraint usage :-> recur
  | Abs (Constraint usage) recur
  | App recur recur
  | If recur recur recur
  | Type usage :* Type usage
  | Pair recur recur
  | Nth Int recur
  | Ann recur (Type usage)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 0 :->
infixl 7 :*


newtype Type usage = Type { unType :: Expr usage (Type usage) }
  deriving (Eq, Ord, Show)

instance Substitutable (Type usage) where
  apply subst ty = case unType ty of
    Unit                            -> Type Unit
    UnitType                        -> Type UnitType
    BoolType                        -> Type BoolType
    T                               -> Type T
    F                               -> Type F
    TypeType                        -> Type TypeType
    Var name                        -> fromMaybe (Type (Var name)) (lookupSubst name subst)
    (name, usage) ::: ty :-> body   -> Type ((name, usage) ::: apply subst ty :-> apply (deleteSubst name subst) body)
    Abs ((name, usage) ::: ty) body -> Type (Abs ((name, usage) ::: apply subst ty) (apply (deleteSubst name subst) body))
    App f a                         -> Type (App (apply subst f) (apply subst a))
    If c t e                        -> Type (If (apply subst c) (apply subst t) (apply subst e))
    a :* b                          -> Type (apply subst a :* apply subst b)
    Pair a b                        -> Type (Pair (apply subst a) (apply subst b))
    Nth i a                         -> Type (Nth i (apply subst a))
    Ann a t                         -> Type (Ann (apply subst a) (apply subst t))


newtype Term usage = Term { unTerm :: Expr usage (Term usage) }
  deriving (Eq, Ord, Show)


data Constraint usage = Binding usage ::: Type usage
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infix 5 :::

type Binding usage = (Name, usage)


instance Bifoldable Expr where
  bifoldMap f g = \case
    Unit         -> mempty
    UnitType     -> mempty
    BoolType     -> mempty
    T            -> mempty
    F            -> mempty
    TypeType     -> mempty
    Var _        -> mempty
    var :-> body -> foldMap f var <> g body
    Abs var body -> foldMap f var <> g body
    App f a      -> g f <> g a
    If c t e     -> g c <> g t <> g e
    a :* b       -> foldMap f a <> foldMap f b
    Pair a b     -> g a <> g b
    Nth _ a      -> g a
    Ann a t      -> g a <> foldMap f t

instance Bifunctor Expr where
  bimap f g = \case
    Unit         -> Unit
    UnitType     -> UnitType
    BoolType     -> BoolType
    T            -> T
    F            -> F
    TypeType     -> TypeType
    Var name     -> Var name
    var :-> body -> fmap f var :-> g body
    Abs var body -> Abs (fmap f var) (g body)
    App f a      -> App (g f) (g a)
    If c t e     -> If (g c) (g t) (g e)
    a :* b       -> fmap f a :* fmap f b
    Pair a b     -> Pair (g a) (g b)
    Nth i a      -> Nth i (g a)
    Ann a t      -> Ann (g a) (fmap f t)

instance Bitraversable Expr where
  bitraverse f g = \case
    Unit         -> pure Unit
    UnitType     -> pure UnitType
    BoolType     -> pure BoolType
    T            -> pure T
    F            -> pure F
    TypeType     -> pure TypeType
    Var name     -> pure (Var name)
    var :-> body -> (:->) <$> traverse f var <*> g body
    Abs var body -> Abs <$> traverse f var <*> g body
    App f a      -> App <$> g f <*> g a
    If c t e     -> If <$> g c <*> g t <*> g e
    a :* b       -> (:*) <$> traverse f a <*> traverse f b
    Pair a b     -> Pair <$> g a <*> g b
    Nth i a      -> Nth i <$> g a
    Ann a t      -> Ann <$> g a <*> traverse f t


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
