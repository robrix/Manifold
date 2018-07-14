{-# LANGUAGE FlexibleContexts, GADTs #-}
module Manifold.Store where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Manifold.Address
import Manifold.Evaluator

newtype Store address value = Store { unStore :: Map.Map address (Set.Set value) }

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


data StoreError address value result where
  Unallocated   :: address -> StoreError address value (Set.Set value)
  Uninitialized :: address -> StoreError address value value
