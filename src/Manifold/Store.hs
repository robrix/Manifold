{-# LANGUAGE FlexibleContexts #-}
module Manifold.Store where

import qualified Data.Map.Monoidal as Monoidal
import Manifold.Address
import Manifold.Evaluator

newtype Store address cell = Store { unStore :: Monoidal.Map address cell }

modifyStore :: (Address address cell effects, Member (State (Store address (cell value))) effects)
            => (Store address (cell value) -> Store address (cell value))
            -> Evaluator address value effects ()
modifyStore = modify'
