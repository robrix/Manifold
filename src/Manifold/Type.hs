{-# LANGUAGE TypeFamilies #-}
module Manifold.Type where

import Data.Bifoldable
import Data.Bifunctor
import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Data.Maybe (fromMaybe)
import Manifold.Constraint
import Manifold.Expr
import Manifold.Name
import Manifold.Pretty
import Manifold.Substitution

newtype Type var = Type { unType :: Expr (Constraint var (Type var)) (Type var) }
  deriving (Eq, Ord, Show)

type instance Base (Type var) = Expr (Constraint var (Type var))

instance Recursive   (Type var) where project = unType
instance Corecursive (Type var) where embed   =   Type

instance Named var => Substitutable (Type var) where
  apply subst ty = case unType ty of
    Var name                      -> fromMaybe (tvar name) (lookupSubst name subst)
    Intro (Abs (var ::: ty) body) -> tintro (Abs (var ::: apply subst ty) (apply (deleteSubst (name var) subst) body))
    Intro i                       -> tintro (apply subst <$> i)
    IntroT (var ::: ty :-> body)  -> tintroT (var ::: apply subst ty :-> apply (deleteSubst (name var) subst) body)
    IntroT i                      -> tintroT (apply subst <$> i)
    Elim e                        -> telim (apply subst <$> e)

instance Pretty var => Pretty (Type var) where
  prettyPrec d = prettyPrec d . unType


tvar :: Name -> Type var
tvar = Type . Var

tintro :: Intro (Constraint var (Type var)) (Type var) (Type var) -> Type var
tintro = Type . Intro

tintroT :: IntroT (Constraint var (Type var)) (Type var) (Type var) -> Type var
tintroT = Type . IntroT

telim :: Elim (Type var) -> Type var
telim = Type . Elim


typeT :: Type var
typeT = tintroT TypeT

unitT :: Type var
unitT = tintroT UnitT

boolT :: Type var
boolT = tintroT BoolT


(.->) :: Constraint var (Type var) -> Type var -> Type var
constraint .-> ty = tintroT (constraint :-> ty)

infixr 0 .->

(.*) :: Type var -> Type var -> Type var
a .* b = tintroT (a :* b)

infixl 7 .*


instance Foldable Type where
  foldMap f = bifoldMap (bifoldMap f (foldMap f)) (foldMap f) . unType

instance Functor Type where
  fmap f = Type . bimap (bimap f (fmap f)) (fmap f) . unType
