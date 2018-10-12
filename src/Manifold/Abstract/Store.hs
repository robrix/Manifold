{-# LANGUAGE DeriveFunctor, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, LambdaCase, PolyKinds, StandaloneDeriving, TypeOperators #-}
module Manifold.Abstract.Store where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Semilattice.Lower
import qualified Data.Set as Set
import Manifold.Abstract.Evaluator
import Manifold.Name
import Manifold.Pretty

newtype Store address value = Store { unStore :: Map.Map address (Set.Set value) }
  deriving (Lower)

alloc :: (Member (Allocator address value) sig, Carrier sig carrier) => Name -> Evaluator address value carrier address
alloc name = sendAllocator (Alloc name gen)

assign :: ( Carrier sig m
          , Member (Allocator address value) sig
          , Member (State (Store address value)) sig
          , Ord address
          )
       => address
       -> value
       -> Evaluator address value m ()
assign address value = do
  Store store <- get
  cell <- sendAllocator (AssignCell value (fromMaybe Set.empty (Map.lookup address store)) gen)
  put (Store (Map.insert address cell store))

deref :: ( Carrier sig m
         , Member (Allocator address value) sig
         , Member (Resumable (StoreError address value)) sig
         , Member (State (Store address value)) sig
         , Ord address
         )
      => address
      -> Evaluator address value m value
deref address = gets (Map.lookup address . unStore) >>= maybe (throwResumable (Unallocated address)) pure >>= sendAllocator . flip DerefCell gen >>= maybe (throwResumable (Uninitialized address)) pure

sendAllocator :: (Member (Allocator address value) sig, Carrier sig carrier) => Allocator address value (Evaluator address value carrier) (Evaluator address value carrier a) -> Evaluator address value carrier a
sendAllocator = send


data Allocator address value m k
  = Alloc Name (address -> k)
  | DerefCell (Set.Set value) (Maybe value -> k)
  | AssignCell value (Set.Set value) (Set.Set value -> k)
  deriving (Functor)

instance HFunctor (Allocator address value) where
  hfmap _ (Alloc name k)            = Alloc name k
  hfmap _ (DerefCell cell k)        = DerefCell cell k
  hfmap _ (AssignCell value cell k) = AssignCell value cell k

instance Effect (Allocator address value) where
  handle state handler (Alloc name k)            = Alloc name (handler . (<$ state) . k)
  handle state handler (DerefCell cell k)        = DerefCell cell (handler . (<$ state) . k)
  handle state handler (AssignCell value cell k) = AssignCell value cell (handler . (<$ state) . k)


runStore :: Effectful sig m => Evaluator address value (StateC (Store address value) m) a -> m (Store address value, a)
runStore = runState lowerBound . runEvaluator

data StoreError address value result where
  Unallocated :: address -> StoreError address value (Set.Set value)
  Uninitialized :: address -> StoreError address value value

instance Pretty address => Pretty1 (StoreError address value) where
  liftPrettyPrec _ d (Unallocated address) = prettyParen (d > 0) $ prettyString "unallocated address:" <+> pretty address
  liftPrettyPrec _ d (Uninitialized address) = prettyParen (d > 0) $ prettyString "Uninitialized address:" <+> pretty address
