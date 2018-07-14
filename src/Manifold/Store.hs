{-# LANGUAGE FlexibleContexts #-}
module Manifold.Store where

import qualified Data.Map.Monoidal as Monoidal
import Manifold.Address
import Manifold.Evaluator

newtype Store address value = Store { unStore :: Monoidal.Map address [value] }

assign :: (Address address effects, Member (State (Store address value)) effects)
       => address
       -> value
       -> Evaluator address value effects ()
assign address value = modifyStore (Store . Monoidal.insert address [value] . unStore)


modifyStore :: Member (State (Store address value)) effects
            => (Store address value -> Store address value)
            -> Evaluator address value effects ()
modifyStore = modify'
