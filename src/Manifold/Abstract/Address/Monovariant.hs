{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, TypeOperators #-}
module Manifold.Abstract.Address.Monovariant where

import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Data.Monoid (Alt(..))
import Data.Semilattice.Lower
import qualified Data.Set as Set
import Manifold.Abstract.Env (Env(..))
import Manifold.Abstract.Evaluator
import Manifold.Abstract.Store
import Manifold.Constraint
import Manifold.Context
import Manifold.Name
import Manifold.Pretty

newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)

instance Pretty Monovariant where
  prettyPrec d (Monovariant n) = prettyParen (d > 10) $ prettyString "Monovariant" <+> pretty n


runEnv :: ( Member (State (Store Monovariant value)) effects
          , PureEffects effects
          , Ord value
          )
       => Evaluator Monovariant value (Env Monovariant ': effects) a
       -> Evaluator Monovariant value effects a
runEnv = interpret $ \case
  Lookup name -> do
    Store store <- getStore
    let addr = Monovariant name
    pure (addr <$ Map.lookup addr store)
  Bind _ addr m -> do
    modifyStore (Store . Map.insertWith (<>) addr Set.empty . unStore)
    runEnv (Evaluator m)
  Close fvs -> pure (foldl (|>) lowerBound (Set.map ((:::) <*> Monovariant) fvs))


getStore :: Member (State (Store Monovariant value)) effects => Evaluator Monovariant value effects (Store Monovariant value)
getStore = get

modifyStore :: Member (State (Store Monovariant value)) effects => (Store Monovariant value -> Store Monovariant value) -> Evaluator Monovariant value effects ()
modifyStore = modify'


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
