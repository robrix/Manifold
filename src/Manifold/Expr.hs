{-# LANGUAGE LambdaCase, TypeFamilies #-}
module Manifold.Expr
( Intro(..)
, Elim(..)
, Expr(..)
, rerep
, bindVariable
, freeVariables
, Silent(..)
) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Foldable (fold)
import Data.Functor.Classes (showsUnaryWith)
import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import qualified Data.Set as Set
import Manifold.Constraint
import Manifold.Expr.Elim
import Manifold.Expr.Intro
import Manifold.Name

data Expr var recur
  = Var Name
  | Intro (Intro var recur recur)
  | Elim (Elim recur)
  deriving (Eq, Ord)

instance Bifoldable Expr where
  bifoldMap f g = \case
    Var _   -> mempty
    Intro i -> trifoldMap f g g i
    Elim e  -> foldMap g e

instance Foldable (Expr var) where
  foldMap = bifoldMap (const mempty)

instance Bifunctor Expr where
  bimap f g = \case
    Var n   -> Var n
    Intro i -> Intro (trimap f g g i)
    Elim e  -> Elim (fmap g e)

instance Functor (Expr var) where
  fmap = bimap id

instance (Show var, Show recur) => Show (Expr var recur) where
  showsPrec d = \case
    Var n   -> showsUnaryWith showsPrec "Var" d n
    Intro i -> showsPrec d i
    Elim e  -> showsPrec d e


rerep :: (Recursive t1, Base t1 ~ f (Constraint var1 t1), Corecursive t2, Base t2 ~ f (Constraint var2 t2), Bifunctor f)
      => (var1 -> var2)
      -> t1
      -> t2
rerep vars = go where go = cata (embed . first (bimap vars go))


bindVariable :: (Corecursive t, Recursive t, Base t ~ Expr (Constraint var t), Named var) => (t -> t) -> (Name, t)
bindVariable f = (n, body)
  where n = maybe (I 0) prime (maxBV body)
        body = f (embed (Var n))
        prime (I i) = I (succ i)
        prime (N s) = N (s <> "ʹ")
        maxBV = cata $ \case
          Intro (var ::: ty :-> _) -> max (Just (name var)) (maxBV ty)
          Intro (Abs (var ::: ty) _) -> max (Just (name var)) (maxBV ty)
          other -> foldr max Nothing other


freeVariables :: (Recursive t, Base t ~ Expr (Constraint var t), Named var) => t -> Set.Set Name
freeVariables = cata $ \case
  Var name -> Set.singleton name
  Intro (var ::: ty :-> body) -> freeVariables ty <> Set.delete (name var) body
  Intro (Abs (var ::: ty) body) -> freeVariables ty <> Set.delete (name var) body
  other -> fold other


newtype Silent var = Silent { unSilent :: Expr (Constraint var (Silent var)) (Silent var) }

type instance Base (Silent var) = Expr (Constraint var (Silent var))

instance Recursive   (Silent var) where project = unSilent
instance Corecursive (Silent var) where embed   =   Silent

instance Show var => Show (Silent var) where
  showsPrec d = showsPrec d . unSilent
