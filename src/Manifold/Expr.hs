{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, LambdaCase, TypeFamilies #-}
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


rerep :: (Recursive t1, Base t1 ~ f (Constraint usage t1), Corecursive t2, Base t2 ~ f (Constraint usage t2), Bifunctor f)
      => t1
      -> t2
rerep = cata (embed . first (fmap rerep))


bindVariable :: (Corecursive t, Recursive t, Base t ~ Expr (Constraint usage t)) => (t -> t) -> (Name, t)
bindVariable f = (name, body)
  where name = maybe (I 0) prime (maxBV body)
        body = f (embed (Var name))
        prime (I i) = I (succ i)
        prime (N s) = N (s <> "ʹ")
        maxBV = cata $ \case
          Intro ((name, _) ::: ty :-> _) -> max (Just name) (maxBV ty)
          Intro (Abs ((name, _) ::: ty) _) -> max (Just name) (maxBV ty)
          other -> foldr max Nothing other


freeVariables :: (Recursive t, Base t ~ Expr (Constraint usage t)) => t -> Set.Set Name
freeVariables = cata $ \case
  Var name -> Set.singleton name
  Intro ((name, _) ::: ty :-> body) -> freeVariables ty <> Set.delete name body
  Intro (Abs ((name, _) ::: ty) body) -> freeVariables ty <> Set.delete name body
  other -> fold other


newtype Silent usage = Silent { unSilent :: Expr (Constraint usage (Silent usage)) (Silent usage) }

type instance Base (Silent usage) = Expr (Constraint usage (Silent usage))

instance Recursive   (Silent usage) where project = unSilent
instance Corecursive (Silent usage) where embed   =   Silent

instance Show usage => Show (Silent usage) where
  showsPrec d = showsPrec d . unSilent
