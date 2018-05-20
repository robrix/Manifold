{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving #-}
module Manifold.Proof where

import Control.Monad.Effect
import Control.Monad.Effect.Exception
import Manifold.Name

newtype Proof usage effects a = Proof { runProof :: Eff effects a }
  deriving (Applicative, Effectful, Functor, Monad)


noRuleTo :: Member (Exc (Some (proposition usage))) effects => proposition usage result -> Proof usage effects a
noRuleTo = throwError . Some

data Some proposition where
  Some :: proposition result -> Some proposition


data Error where
  FreeVariable :: Name -> Error
