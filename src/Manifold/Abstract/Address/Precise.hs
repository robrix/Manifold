{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, TypeOperators #-}
module Manifold.Abstract.Address.Precise where

import Data.Monoid (Last(..))
import qualified Data.Set as Set
import Manifold.Abstract.Evaluator
import Manifold.Abstract.Store
import Manifold.Pretty

newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

instance Pretty Precise where
  prettyPrec d (Precise i) = prettyParen (d > 10) $ prettyString "Precise" <+> pretty i


runAllocator :: ( Member Fresh effects
                , PureEffects effects
                )
             => Evaluator Precise value (Allocator Precise value ': effects) a
             -> Evaluator Precise value effects a
runAllocator = interpret $ \case
  Alloc _            -> Precise <$> fresh
  DerefCell cell     -> pure (getLast (foldMap (Last . Just) cell))
  AssignCell value _ -> pure (Set.singleton value)
