{-# LANGUAGE TypeFamilies #-}
module Manifold.Type where

import Data.Bifoldable
import Data.Bifunctor
import Data.Functor.Classes (showsUnaryWith)
import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Data.Maybe (fromMaybe)
import Manifold.Expr
import Manifold.Name
import Manifold.Substitution

newtype Type usage = Type { unType :: Expr (Constraint usage (Type usage)) (Type usage) }
  deriving (Eq, Ord)

instance Show usage => Show (Type usage) where
  showsPrec d = showsUnaryWith showsPrec "Type" d . unSilent . rerep

type instance Base (Type usage) = Expr (Constraint usage (Type usage))

instance Recursive   (Type usage) where project = unType
instance Corecursive (Type usage) where embed   =   Type

instance Substitutable (Type usage) where
  apply subst ty = case unType ty of
    Var name                                -> fromMaybe (tvar name) (lookupSubst name subst)
    Intro ((name, usage) ::: ty :-> body)   -> tintro ((name, usage) ::: apply subst ty :-> apply (deleteSubst name subst) body)
    Intro (Abs ((name, usage) ::: ty) body) -> tintro (Abs ((name, usage) ::: apply subst ty) (apply (deleteSubst name subst) body))
    Intro i                                 -> tintro (apply subst <$> i)
    Elim e                                  -> telim (apply subst <$> e)

tvar :: Name -> Type usage
tvar = Type . Var

tintro :: Intro (Constraint usage (Type usage)) (Type usage) (Type usage) -> Type usage
tintro = Type . Intro

telim :: Elim (Type usage) -> Type usage
telim = Type . Elim


typeT :: Type usage
typeT = tintro TypeT

unitT :: Type usage
unitT = tintro UnitT

boolT :: Type usage
boolT = tintro BoolT


(.->) :: Constraint usage (Type usage) -> Type usage -> Type usage
constraint .-> ty = tintro (constraint :-> ty)

infixr 0 .->

(.*) :: Type usage -> Type usage -> Type usage
a .* b = tintro (a :* b)

infixl 7 .*


instance Foldable Type where
  foldMap f = bifoldMap (bifoldMap f (foldMap f)) (foldMap f) . unType

instance Functor Type where
  fmap f = Type . bimap (bimap f (fmap f)) (fmap f) . unType
