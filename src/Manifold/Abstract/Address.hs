{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Manifold.Abstract.Address where

import Data.List.NonEmpty (nonEmpty)
import Data.Monoid (Alt(..), Last(..))
import qualified Data.Set as Set
import Manifold.Abstract.Evaluator
import Manifold.Name

class (Ord address, Show address) => Address address effects where
  alloc :: Name -> Evaluator address value effects address
  derefCell :: address -> Set.Set value -> Evaluator address value effects (Maybe value)
  assignCell :: Ord value => address -> value -> Set.Set value -> Evaluator address value effects (Set.Set value)


newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

instance Member Fresh effects => Address Precise effects where
  alloc _ = Precise <$> fresh
  derefCell _ cell = pure (getLast (foldMap (Last . Just) cell))
  assignCell _ value _ = pure (Set.singleton value)



newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)

instance Member NonDet effects => Address Monovariant effects where
  alloc = pure . Monovariant
  derefCell _ = traverse (foldMapA pure) . nonEmpty . Set.toList
  assignCell _ value cell = pure (Set.insert value cell)


foldMapA :: (Alternative m, Foldable t) => (b -> m a) -> t b -> m a
foldMapA f = getAlt . foldMap (Alt . f)
