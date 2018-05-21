{-# LANGUAGE TypeFamilies #-}
module Manifold.Value where

import Data.Functor.Classes (showsUnaryWith)
import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Manifold.Context
import Manifold.Expr.Intro
import Manifold.Name
import Manifold.Term

newtype Value usage = Value { unValue :: Intro (Name, Context usage (Value usage)) Term (Value usage) }
  deriving (Eq, Ord)

instance Show usage => Show (Value usage) where
  showsPrec d = showsUnaryWith showsPrec "Value" d . cata Silent

type instance Base (Value usage) = Intro (Name, Context usage (Value usage)) Term

instance Recursive   (Value usage) where project = unValue
instance Corecursive (Value usage) where embed   =   Value


newtype Silent usage = Silent { unSilent :: Intro (Name, Context usage (Value usage)) Term (Silent usage) }

type instance Base (Silent usage) = Intro (Name, Context usage (Value usage)) Term

instance Recursive   (Silent usage) where project = unSilent
instance Corecursive (Silent usage) where embed   =   Silent

instance Show usage => Show (Silent usage) where
  showsPrec d = showsPrec d . unSilent
