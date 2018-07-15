{-# LANGUAGE GADTs, KindSignatures, MultiParamTypeClasses #-}
module Manifold.Abstract.Value where

import Manifold.Abstract.Evaluator
import Manifold.Name
import Manifold.Term

class Value address value effects where
  lambda :: Name -> Term Name -> Evaluator address value effects value
  apply :: value -> value -> Evaluator address value effects value

  construct :: Name -> [value] -> Evaluator address value effects value
  deconstruct :: value -> Evaluator address value effects (Name, [value])


data Function value (m :: * -> *) result where
  Lambda :: Name -> m a -> Function value m a
  Apply  :: value -> value -> Function value m value
