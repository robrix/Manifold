{-# LANGUAGE TypeFamilies #-}
module Manifold.Value where

import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Manifold.Constraint
import Manifold.Context
import Manifold.Name
import Manifold.Pretty
import Manifold.Term (Term)
import Manifold.Value.Intro

newtype Value = Value { unValue :: Intro (Constraint Name (Context Name Value)) (Term Name) Value }
  deriving (Eq, Ord, Show)

value :: Intro (Constraint Name (Context Name Value)) (Term Name) Value -> Value
value = Value


type instance Base Value = Intro (Constraint Name (Context Name Value)) (Term Name)

instance Recursive   Value where project = unValue
instance Corecursive Value where embed   =   Value

instance Pretty Value where
  prettyPrec d = prettyPrec d . unValue
