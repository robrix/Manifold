{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
module Manifold.Address where

import Data.List.NonEmpty (nonEmpty)
import Data.Monoid (Alt(..), Last(..))
import Manifold.Evaluator
import Manifold.Name
import qualified Data.Set as Set

class Address address cell effects | address -> cell where
  allocCell :: Name -> Evaluator (address value) effects (address value)
  derefCell :: address value -> cell value -> Evaluator (address value) effects (Maybe value)


newtype Precise value = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

instance Member Fresh effects => Address Precise Last effects where
  allocCell _ = Precise <$> fresh
  derefCell _ cell = pure (getLast cell)



newtype Monovariant value = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)

instance Member NonDet effects => Address Monovariant Set.Set effects where
  allocCell = pure . Monovariant
  derefCell _ = traverse (foldMapA pure) . nonEmpty . Set.toList


foldMapA :: (Alternative m, Foldable t) => (b -> m a) -> t b -> m a
foldMapA f = getAlt . foldMap (Alt . f)
