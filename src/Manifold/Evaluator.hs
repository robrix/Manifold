{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Manifold.Evaluator where

import Control.Monad.Effect

newtype Evaluator address effects a = Evaluator { runEvaluator :: Eff effects a }
  deriving (Applicative, Functor, Monad)
