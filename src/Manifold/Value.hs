{-# LANGUAGE TypeFamilies #-}
module Manifold.Value where

import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Manifold.Constraint
import Manifold.Context
import Manifold.Expr.Intro
import Manifold.Name
import Manifold.Pretty
import Manifold.Term

newtype Value = Value { unValue :: Intro (Constraint Name (Context Name Value)) Term Value }
  deriving (Eq, Ord, Show)

type instance Base Value = Intro (Constraint Name (Context Name Value)) Term

instance Recursive   Value where project = unValue
instance Corecursive Value where embed   =   Value

instance Pretty Value where
  prettyPrec d = prettyPrec d . unValue
