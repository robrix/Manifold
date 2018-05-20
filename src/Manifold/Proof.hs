{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving #-}
module Manifold.Proof where

import Control.Monad.Effect

newtype Proof usage effects a = Proof { runProof :: Eff effects a }
  deriving (Applicative, Effectful, Functor, Monad)
