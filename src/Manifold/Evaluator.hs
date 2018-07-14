{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Manifold.Evaluator
( Evaluator(..)
, Alternative(..)
, module X
) where

import Control.Applicative
import Control.Monad.Effect as X
import Control.Monad.Effect.Fresh as X
import Control.Monad.Effect.NonDet as X
import Control.Monad.Effect.Reader as X
import Control.Monad.Effect.Resumable as X

newtype Evaluator address effects a = Evaluator { runEvaluator :: Eff effects a }
  deriving (Applicative, Functor, Monad)

deriving instance Member NonDet effects => Alternative (Evaluator address effects)
