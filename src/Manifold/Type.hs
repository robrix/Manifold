{-# LANGUAGE LambdaCase, TypeFamilies #-}
module Manifold.Type where

import Data.Bifoldable
import Data.Bifunctor
import Data.Foldable (fold, toList)
import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Trifoldable
import Data.Trifunctor
import Manifold.Constraint
import Manifold.Name
import Manifold.Pretty
import Manifold.Substitution
import Manifold.Term.Elim
import Manifold.Term.Intro
import Manifold.Type.Intro

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
unitT = typeC (N "Unit") []

boolT :: Type var
boolT = typeC (N "Bool") []

typeC :: Name -> [Type var] -> Type var
typeC c ts = tintroT (TypeC c ts)


(.->) :: Constraint var (Type var) -> Type var -> Type var
constraint .-> ty = tintroT (constraint :-> ty)

infixr 0 .->

(.*) :: Type var -> Type var -> Type var
a .* b = tintroT (a :* b)

infixl 7 .*


(#) :: Type var -> Type var -> Type var
f # a = telim (App f a)

infixl 9 #


freeTypeVariables :: Named var => Type var -> Set.Set Name
freeTypeVariables = cata $ \case
  Var name -> Set.singleton name
  Intro (Abs (var ::: ty) rest) -> Set.delete (name var) (freeTypeVariables ty <> rest)
  Intro i -> bifold i
  IntroT (var ::: ty :-> rest) -> Set.delete (name var) (freeTypeVariables ty <> rest)
  IntroT i -> bifold i
  Elim e -> fold e

generalize :: Type Name -> Type Name
generalize ty = foldr ((.->) . (::: typeT)) ty fvs
  where fvs = toList (freeTypeVariables ty)


instance Foldable Type where
  foldMap f = bifoldMap (bifoldMap f (foldMap f)) (foldMap f) . unType

instance Functor Type where
  fmap f = Type . bimap (bimap f (fmap f)) (fmap f) . unType


data Expr var recur
  = Var Name
  | Intro (Intro var recur recur)
  | IntroT (IntroT var recur recur)
  | Elim (Elim recur)
  deriving (Eq, Ord, Show)

instance Bifoldable Expr where
  bifoldMap f g = \case
    Var _   -> mempty
    Intro i -> trifoldMap f g g i
    IntroT i -> trifoldMap f g g i
    Elim e  -> foldMap g e

instance Foldable (Expr var) where
  foldMap = bifoldMap (const mempty)

instance Bifunctor Expr where
  bimap f g = \case
    Var n   -> Var n
    Intro i -> Intro (trimap f g g i)
    IntroT i -> IntroT (trimap f g g i)
    Elim e  -> Elim (fmap g e)

instance Functor (Expr var) where
  fmap = bimap id

instance (Pretty var, Pretty recur) => Pretty (Expr var recur) where
  prettyPrec d (Var n)    = prettyPrec d n
  prettyPrec d (Intro  i) = prettyPrec d i
  prettyPrec d (IntroT i) = prettyPrec d i
  prettyPrec d (Elim e)   = prettyPrec d e
