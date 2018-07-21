{-# LANGUAGE FlexibleContexts, GADTs, KindSignatures, MultiParamTypeClasses #-}
module Manifold.Abstract.Value where

import Manifold.Abstract.Evaluator
import Manifold.Name

class Value address value effects where
  construct :: Name -> [value] -> Evaluator address value effects value
  deconstruct :: value -> Evaluator address value effects (Name, [value])


lambda :: Member (Function value) effects => Name -> Evaluator address value effects value -> Evaluator address value effects value
lambda name body = send (Lambda name (lowerEff body))

apply :: Member (Function value) effects => value -> value -> Evaluator address value effects value
apply f a = send (Apply f a)

data Function value m result where
  Lambda :: Name -> m value -> Function value m value
  Apply  :: value -> value -> Function value m value

instance PureEffect (Function value) where
  handle handler (Request (Lambda n m) k) = Request (Lambda n (handler m)) (handler . k)
  handle handler (Request (Apply f a)  k) = Request (Apply f a)            (handler . k)

data Data value (m :: * -> *) result where
  Construct   :: Name -> [value] -> Data value m value
  Deconstruct :: value -> Data value m (Name, [value])
