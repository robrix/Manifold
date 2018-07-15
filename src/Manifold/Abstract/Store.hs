{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, TypeOperators #-}
module Manifold.Abstract.Store where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Semilattice.Lower
import qualified Data.Set as Set
import Manifold.Abstract.Address
import Manifold.Abstract.Evaluator

newtype Store address value = Store { unStore :: Map.Map address (Set.Set value) }
  deriving (Lower)

assign :: (Address address effects, Member (State (Store address value)) effects, Ord value)
       => address
       -> value
       -> Evaluator address value effects ()
assign address value = do
  Store store <- get
  cell <- assignCell address value (fromMaybe Set.empty (Map.lookup address store))
  put (Store (Map.insert address cell store))

deref :: ( Address address effects
         , Member (Resumable (StoreError address value)) effects
         , Member (State (Store address value)) effects
         )
      => address
      -> Evaluator address value effects value
deref address = gets (Map.lookup address . unStore) >>= maybe (throwResumable (Unallocated address)) pure >>= derefCell address >>= maybe (throwResumable (Uninitialized address)) pure


runStore :: Effects effects => Evaluator address value (State (Store address value) ': effects) a -> Evaluator address value effects (Store address value, a)
runStore = runState lowerBound

data StoreError address value result where
  Unallocated   :: address -> StoreError address value (Set.Set value)
  Uninitialized :: address -> StoreError address value value
