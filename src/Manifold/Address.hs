{-# LANGUAGE FunctionalDependencies #-}
module Manifold.Address where

import Manifold.Evaluator
import Manifold.Name

class Address address cell effects | address -> cell where
  allocCell :: Name -> Evaluator (address value) effects (address value)
  derefCell :: address value -> cell value -> Evaluator (address value) effects (Maybe value)

newtype Precise value = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

newtype Monovariant value = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)
