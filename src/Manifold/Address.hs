{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Manifold.Address where

import Data.List.NonEmpty (nonEmpty)
import Data.Monoid (Alt(..), Last(..))
import Manifold.Evaluator
import Manifold.Name

class (Ord address, Show address) => Address address effects where
  alloc :: Name -> Evaluator address value effects address
  derefCell :: address -> [value] -> Evaluator address value effects (Maybe value)


newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

instance Member Fresh effects => Address Precise effects where
  alloc _ = Precise <$> fresh
  derefCell _ cell = pure (getLast (foldMap (Last . Just) cell))



newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)

instance Member NonDet effects => Address Monovariant effects where
  alloc = pure . Monovariant
  derefCell _ = traverse (foldMapA pure) . nonEmpty


foldMapA :: (Alternative m, Foldable t) => (b -> m a) -> t b -> m a
foldMapA f = getAlt . foldMap (Alt . f)
