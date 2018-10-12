{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, UndecidableInstances #-}
module Manifold.Abstract.Evaluator
( Evaluator(..)
, Alternative(..)
, module X
) where

import Control.Applicative
import Control.Effect as X

newtype Evaluator address value carrier a = Evaluator { runEvaluator :: Eff carrier a }
  deriving (Applicative, Functor, Monad)

deriving instance (Member NonDet sig, Carrier sig carrier) => Alternative (Evaluator address value carrier)

instance Carrier sig carrier => Carrier sig (Evaluator address value carrier) where
  gen = pure
  alg op = Evaluator (alg (handlePure runEvaluator op))

instance (Carrier sig carrier, Effect sig) => Effectful sig (Evaluator address value carrier)
