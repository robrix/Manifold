{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators #-}
module Manifold.Abstract.Env where

import Manifold.Constraint
import Manifold.Context
import Manifold.Abstract.Evaluator
import Manifold.Name
import Data.Semilattice.Lower

type Env address = Context (Constraint Name address)


askEnv :: Member (Reader (Env address)) effects => Evaluator address value effects (Env address)
askEnv = ask

lookupEnv :: (Member (Reader (Env address)) effects, Member (Resumable (EnvError address)) effects) => Name -> Evaluator address value effects address
lookupEnv name = askEnv >>= maybe (throwResumable (FreeVariable name)) (pure . constraintValue) . contextLookup name


runEnv :: Effects effects => Evaluator address value (Reader (Env address) ': effects) a -> Evaluator address value effects a
runEnv = runReader lowerBound


(.=) :: Member (Reader (Env address)) effects => Name -> address -> Evaluator address value effects a -> Evaluator address value effects a
name .= value = local (|> (name ::: value))

infixl 1 .=


data EnvError address result where
  FreeVariable :: Name -> EnvError address address
