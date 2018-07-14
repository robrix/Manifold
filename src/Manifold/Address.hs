{-# LANGUAGE FunctionalDependencies #-}
module Manifold.Address where

import Manifold.Evaluator
import Manifold.Name

class Address address cell | address -> cell where
  alloc :: Name -> Evaluator (address value) effects (address value)
  deref :: address value -> cell value -> Evaluator (address value) effects value

newtype Precise value = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

newtype Monovariant value = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)
