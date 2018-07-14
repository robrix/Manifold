{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Manifold.Evaluator
( Evaluator(..)
, module X
) where

import Control.Monad.Effect as X
import Control.Monad.Effect.Reader as X

newtype Evaluator address effects a = Evaluator { runEvaluator :: Eff effects a }
  deriving (Applicative, Functor, Monad)
