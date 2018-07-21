{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, TypeOperators #-}
module Manifold.Abstract.Store where

import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Last(..))
import Data.Semilattice.Lower
import qualified Data.Set as Set
import Manifold.Abstract.Address
import Manifold.Abstract.Evaluator
import Manifold.Name
import Manifold.Pretty

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


data Allocator address value (m :: * -> *) result where
  Alloc      :: Name                              -> Allocator address value m address
  Deref      :: address                           -> Allocator address value m value
  AssignCell :: address -> value -> Set.Set value -> Allocator address value m ()

instance PureEffect (Allocator address value)
instance Effect (Allocator address value) where
  handleState state handler (Request (Alloc name) k) = Request (Alloc name) (handler . (<$ state) . k)
  handleState state handler (Request (Deref addr) k) = Request (Deref addr) (handler . (<$ state) . k)
  handleState state handler (Request (AssignCell address value cell) k) = Request (AssignCell address value cell) (handler . (<$ state) . k)

runAllocatorPrecise :: ( Member Fresh effects
                       , Member (Resumable (StoreError Precise value)) effects
                       , Member (State (Store Precise value)) effects
                       , PureEffects effects
                       )
                    => Evaluator Precise value (Allocator Precise value ': effects) a
                    -> Evaluator Precise value effects a
runAllocatorPrecise = interpret $ \case
  Alloc _                 -> Precise <$> fresh
  Deref addr              -> gets (Map.lookup addr . unStore) >>= maybe (throwResumable (Unallocated addr)) pure >>= maybe (throwResumable (Uninitialized addr)) pure . getLast . foldMap (Last . Just)
  AssignCell addr value _ -> do
    Store store <- get
    put (Store (Map.insert addr (Set.singleton value) store))

runAllocatorMonovariant :: ( Member NonDet effects
                           , Member (Resumable (StoreError Monovariant value)) effects
                           , Member (State (Store Monovariant value)) effects
                           , Ord value
                           , PureEffects effects
                           )
                        => Evaluator Monovariant value (Allocator Monovariant value ': effects) a
                        -> Evaluator Monovariant value effects a
runAllocatorMonovariant = interpret $ \case
  Alloc name                 -> pure (Monovariant name)
  Deref addr                 -> gets (Map.lookup addr . unStore) >>= maybe (throwResumable (Unallocated addr)) pure >>= traverse (foldMapA pure) . nonEmpty . Set.toList >>= maybe (throwResumable (Uninitialized addr)) pure
  AssignCell addr value cell ->  do
    Store store <- get
    put (Store (Map.insert addr (Set.insert value cell) store))


runStore :: Effects effects => Evaluator address value (State (Store address value) ': effects) a -> Evaluator address value effects (Store address value, a)
runStore = runState lowerBound

data StoreError address value result where
  Unallocated   :: address -> StoreError address value (Set.Set value)
  Uninitialized :: address -> StoreError address value value

instance Pretty address => Pretty1 (StoreError address value) where
  liftPrettyPrec _ d (Unallocated address) = prettyParen (d > 0) $ prettyString "unallocated address:" <+> pretty address
  liftPrettyPrec _ d (Uninitialized address) = prettyParen (d > 0) $ prettyString "Uninitialized address:" <+> pretty address
