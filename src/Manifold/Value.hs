{-# LANGUAGE TypeFamilies #-}
module Manifold.Value where

import Data.Functor.Classes (showsUnaryWith)
import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Manifold.Constraint
import Manifold.Context
import Manifold.Expr.Intro
import Manifold.Name
import Manifold.Term

newtype Value = Value { unValue :: Intro (Constraint Name (Context Name Value)) Term Value }
  deriving (Eq, Ord)

instance Show Value where
  showsPrec d = showsUnaryWith showsPrec "Value" d . cata Silent

type instance Base Value = Intro (Constraint Name (Context Name Value)) Term

instance Recursive   Value where project = unValue
instance Corecursive Value where embed   =   Value


newtype Silent = Silent { unSilent :: Intro (Constraint Name (Context Name Value)) Term Silent }

type instance Base Silent = Intro (Constraint Name (Context Name Value)) Term

instance Recursive   Silent where project = unSilent
instance Corecursive Silent where embed   =   Silent

instance Show Silent where
  showsPrec d = showsPrec d . unSilent
