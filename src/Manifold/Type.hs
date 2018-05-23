{-# LANGUAGE TypeFamilies #-}
module Manifold.Type where

import Data.Bifoldable
import Data.Bifunctor
import Data.Functor.Classes (showsUnaryWith)
import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Data.Maybe (fromMaybe)
import Manifold.Constraint
import Manifold.Expr
import Manifold.Name
import Manifold.Pretty
import Manifold.Substitution

newtype Type var = Type { unType :: Expr (Constraint var (Type var)) (Type var) }
  deriving (Eq, Ord)

instance Show var => Show (Type var) where
  showsPrec d = showsUnaryWith showsPrec "Type" d . unSilent . rerep id

type instance Base (Type var) = Expr (Constraint var (Type var))

instance Recursive   (Type var) where project = unType
instance Corecursive (Type var) where embed   =   Type

instance Named var => Substitutable (Type var) where
  apply subst ty = case unType ty of
    Var name                      -> fromMaybe (tvar name) (lookupSubst name subst)
    Intro (var ::: ty :-> body)   -> tintro (var ::: apply subst ty :-> apply (deleteSubst (name var) subst) body)
    Intro (Abs (var ::: ty) body) -> tintro (Abs (var ::: apply subst ty) (apply (deleteSubst (name var) subst) body))
    Intro i                       -> tintro (apply subst <$> i)
    Elim e                        -> telim (apply subst <$> e)

instance Pretty var => Pretty (Type var) where
  prettyPrec d = prettyPrec d . unType


tvar :: Name -> Type var
tvar = Type . Var

tintro :: Intro (Constraint var (Type var)) (Type var) (Type var) -> Type var
tintro = Type . Intro

telim :: Elim (Type var) -> Type var
telim = Type . Elim


typeT :: Type var
typeT = tintro TypeT

unitT :: Type var
unitT = tintro UnitT

boolT :: Type var
boolT = tintro BoolT


(.->) :: Constraint var (Type var) -> Type var -> Type var
constraint .-> ty = tintro (constraint :-> ty)

infixr 0 .->

(.*) :: Type var -> Type var -> Type var
a .* b = tintro (a :* b)

infixl 7 .*


instance Foldable Type where
  foldMap f = bifoldMap (bifoldMap f (foldMap f)) (foldMap f) . unType

instance Functor Type where
  fmap f = Type . bimap (bimap f (fmap f)) (fmap f) . unType
