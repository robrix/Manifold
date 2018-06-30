{-# LANGUAGE TypeFamilies #-}
module Manifold.Value where

import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Manifold.Constraint
import Manifold.Context
import Manifold.Name
import Manifold.Pretty
import Manifold.Term (Term)
import Manifold.Value.Intro

type Environment = Context (Constraint Name Value)

newtype Value = Value { unValue :: Intro (Constraint Name Environment) (Term Name) Value }
  deriving (Eq, Ord, Show)

value :: Intro (Constraint Name Environment) (Term Name) Value -> Value
value = Value


type instance Base Value = Intro (Constraint Name Environment) (Term Name)

instance Recursive   Value where project = unValue
instance Corecursive Value where embed   =   Value

instance Pretty Value where
  prettyPrec d = prettyPrec d . unValue
