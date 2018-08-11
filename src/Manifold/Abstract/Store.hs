{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, TypeOperators #-}
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

alloc :: Member (Allocator address value) effects => Name -> Evaluator address value effects address
alloc = sendAllocator . Alloc

assign :: ( Member (Allocator address value) effects
          , Member (State (Store address value)) effects
          , Ord address
          )
       => address
       -> value
       -> Evaluator address value effects ()
assign address value = do
  Store store <- get
  cell <- sendAllocator (AssignCell value (fromMaybe Set.empty (Map.lookup address store)))
  put (Store (Map.insert address cell store))

deref :: ( Member (Allocator address value) effects
         , Member (Resumable (StoreError address value)) effects
         , Member (State (Store address value)) effects
         , Ord address
         )
      => address
      -> Evaluator address value effects value
deref address = gets (Map.lookup address . unStore) >>= maybe (throwResumable (Unallocated address)) pure >>= sendAllocator . DerefCell >>= maybe (throwResumable (Uninitialized address)) pure

sendAllocator :: Member (Allocator address value) effects => Allocator address value (Eff effects) a -> Evaluator address value effects a
sendAllocator = send


data Allocator address value (m :: * -> *) result where
  Alloc      :: Name                   -> Allocator address value m address
  DerefCell  :: Set.Set value          -> Allocator address value m (Maybe value)
  AssignCell :: value -> Set.Set value -> Allocator address value m (Set.Set value)

instance PureEffect (Allocator address value)
instance Effect (Allocator address value) where
  handleState state handler (Request (Alloc name) k) = Request (Alloc name) (handler . (<$ state) . k)
  handleState state handler (Request (DerefCell cell) k) = Request (DerefCell cell) (handler . (<$ state) . k)
  handleState state handler (Request (AssignCell value cell) k) = Request (AssignCell value cell) (handler . (<$ state) . k)


runStore :: Effects effects => Evaluator address value (State (Store address value) ': effects) a -> Evaluator address value effects (Store address value, a)
runStore = runState lowerBound

data StoreError address value result where
  Unallocated   :: address -> StoreError address value (Set.Set value)
  Uninitialized :: address -> StoreError address value value

instance Pretty address => Pretty1 (StoreError address value) where
  liftPrettyPrec _ d (Unallocated address) = prettyParen (d > 0) $ prettyString "unallocated address:" <+> pretty address
  liftPrettyPrec _ d (Uninitialized address) = prettyParen (d > 0) $ prettyString "Uninitialized address:" <+> pretty address
