{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
module Manifold.Env where

import Manifold.Constraint
import Manifold.Context
import Manifold.Evaluator
import Manifold.Name
import Data.Semilattice.Lower

type Env addr = Context (Constraint Name addr)


askEnv :: Member (Reader (Env addr)) effects => Evaluator addr effects (Env addr)
askEnv = ask


runEnv :: Effects effects => Evaluator addr (Reader (Env addr) ': effects) a -> Evaluator addr effects a
runEnv = runReader lowerBound


(.=) :: Member (Reader (Env addr)) effects => Name -> addr -> Evaluator addr effects a -> Evaluator addr effects a
name .= value = local (|> (name ::: value))

infixl 1 .=
