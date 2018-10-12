{-# LANGUAGE FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Manifold.Abstract.Address.Precise
( Precise(..)
, runEnv
, EnvC(..)
, runAllocator
, AllocatorC(..)
) where

import Data.Monoid (Last(..))
import qualified Data.Set as Set
import Manifold.Abstract.Env (Env(..), Environment)
import Manifold.Abstract.Evaluator
import Manifold.Abstract.Store
import Manifold.Constraint
import Manifold.Context
import Manifold.Name
import Manifold.Pretty

newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

instance Pretty Precise where
  prettyPrec d (Precise i) = prettyParen (d > 10) $ prettyString "Precise" <+> pretty i


runEnv :: (HFunctor sig, Carrier (Reader (Environment Precise) :+: sig) m)
       => Evaluator Precise value (EnvC (Evaluator Precise value m)) a
       -> Evaluator Precise value m a
runEnv = runEnvC . interpret . runEvaluator

newtype EnvC m a = EnvC { runEnvC :: m a }

instance (Carrier (Reader (Environment Precise) :+: sig) m, HFunctor sig) => Carrier (Env Precise :+: sig) (EnvC (Evaluator Precise value m)) where
  gen = EnvC . gen
  alg = algE \/ (EnvC . alg . R . handlePure runEnvC)
    where algE (Lookup name      k) = EnvC (askEnv >>= runEnvC . k . fmap constraintValue . contextLookup name)
          algE (Bind name addr m k) = EnvC (local (|> (name ::: addr)) (runEnvC m) >>= runEnvC . k)
          algE (Close fvs        k) = EnvC (askEnv >>= runEnvC . k . contextFilter ((`elem` fvs) . name))


askEnv :: (Member (Reader (Environment address)) sig, Carrier sig carrier) => Evaluator address value carrier (Environment address)
askEnv = ask


runAllocator :: ( Member Fresh sig
                , Carrier sig m
                )
             => Evaluator address value (AllocatorC (Evaluator address value m)) a
             -> Evaluator address value m a
runAllocator = runAllocatorC . interpret . runEvaluator

newtype AllocatorC m a = AllocatorC { runAllocatorC :: m a }

instance (Carrier sig m, Member Fresh sig) => Carrier (Allocator Precise value :+: sig) (AllocatorC (Evaluator address value m)) where
  gen = AllocatorC . gen
  alg = algA \/ (AllocatorC . alg . handlePure runAllocatorC)
    where algA (Alloc _            k) = AllocatorC (fresh >>= runAllocatorC . k . Precise)
          algA (DerefCell cell     k) = k (getLast (foldMap (Last . Just) cell))
          algA (AssignCell value _ k) = k (Set.singleton value)
