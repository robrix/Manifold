{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
module Manifold.Env where

import Manifold.Constraint
import Manifold.Context
import Manifold.Evaluator
import Manifold.Name
import Data.Semilattice.Lower

type Env address = Context (Constraint Name address)


askEnv :: Member (Reader (Env address)) effects => Evaluator address value effects (Env address)
askEnv = ask


runEnv :: Effects effects => Evaluator address value (Reader (Env address) ': effects) a -> Evaluator address value effects a
runEnv = runReader lowerBound


(.=) :: Member (Reader (Env address)) effects => Name -> address -> Evaluator address value effects a -> Evaluator address value effects a
name .= value = local (|> (name ::: value))

infixl 1 .=
