{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, TypeOperators #-}
module Manifold.Abstract.Address.Monovariant where

import Data.List.NonEmpty (nonEmpty)
import Data.Monoid (Alt(..))
import qualified Data.Set as Set
import Manifold.Abstract.Evaluator
import Manifold.Abstract.Store
import Manifold.Name
import Manifold.Pretty

newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)

instance Pretty Monovariant where
  prettyPrec d (Monovariant n) = prettyParen (d > 10) $ prettyString "Monovariant" <+> pretty n


runAllocator :: ( Member NonDet effects
                , Ord value
                , PureEffects effects
                )
             => Evaluator Monovariant value (Allocator Monovariant value ': effects) a
             -> Evaluator Monovariant value effects a
runAllocator = interpret $ \case
  Alloc name            -> pure (Monovariant name)
  DerefCell cell        -> traverse (foldMapA pure) (nonEmpty (Set.toList cell))
  AssignCell value cell -> pure (Set.insert value cell)

foldMapA :: (Alternative m, Foldable t) => (b -> m a) -> t b -> m a
foldMapA f = getAlt . foldMap (Alt . f)
