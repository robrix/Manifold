{-# LANGUAGE FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Manifold.Abstract.Address.Monovariant
( Monovariant(..)
, runEnv
, EnvC(..)
, runAllocator
, AllocatorC(..)
) where

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


runEnv :: ( Member (State (Store Monovariant value)) sig
          , Carrier sig m
          , Ord value
          )
       => Evaluator Monovariant value (EnvC (Evaluator Monovariant value m)) a
       -> Evaluator Monovariant value m a
runEnv = runEnvC . interpret . runEvaluator

newtype EnvC m a = EnvC { runEnvC :: m a }

instance (Carrier sig m, Member (State (Store Monovariant value)) sig, Ord value) => Carrier (Env Monovariant :+: sig) (EnvC (Evaluator Monovariant value m)) where
  gen = EnvC . gen
  alg = algE \/ (EnvC . alg . handlePure runEnvC)
    where algE (Lookup name k) = EnvC $ do
            Store store <- getStore
            let addr = Monovariant name
            runEnvC (k (addr <$ Map.lookup addr store))
          algE (Bind _ addr m k) = EnvC $ do
            modifyStore (Store . Map.insertWith (<>) addr Set.empty . unStore)
            runEnvC m >>= runEnvC . k
          algE (Close fvs k) = k (foldl (|>) lowerBound (Set.map ((:::) <*> Monovariant) fvs))


getStore :: (Member (State (Store Monovariant value)) sig, Carrier sig m) => Evaluator Monovariant value m (Store Monovariant value)
getStore = get

modifyStore :: (Member (State (Store Monovariant value)) sig, Carrier sig m) => (Store Monovariant value -> Store Monovariant value) -> Evaluator Monovariant value m ()
modifyStore = modify


runAllocator :: ( Member NonDet sig
                , Ord value
                , Carrier sig m
                )
             => Evaluator Monovariant value (AllocatorC value (Evaluator Monovariant value m)) a
             -> Evaluator Monovariant value m a
runAllocator = runAllocatorC . interpret . runEvaluator

newtype AllocatorC value m a = AllocatorC { runAllocatorC :: m a }

instance (Alternative m, Carrier sig m, Ord value, Monad m) => Carrier (Allocator Monovariant value :+: sig) (AllocatorC value m) where
  gen = AllocatorC . gen
  alg = algA \/ (AllocatorC . alg . handlePure runAllocatorC)
    where algA (Alloc name k)            = k (Monovariant name)
          algA (DerefCell cell k)        = AllocatorC (traverse (foldMapA pure) (nonEmpty (Set.toList cell)) >>= runAllocatorC . k)
          algA (AssignCell value cell k) = k (Set.insert value cell)

foldMapA :: (Alternative m, Foldable t) => (b -> m a) -> t b -> m a
foldMapA f = getAlt . foldMap (Alt . f)
