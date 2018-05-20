{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators #-}
module Manifold.Unification where

import Control.Monad.Effect
import Manifold.Presyntax
import Manifold.Proof

unify :: Member (Unify usage) effects => Type usage -> Type usage -> Proof usage effects (Type usage)
unify actual expected = send (Unify actual expected)

unification :: ( Eq usage
               , Members '[ Exc (Some (Unify usage))
                          , Unify usage
                          ] effects
               )
            => Unify usage result
            -> Proof usage effects result
unification (Unify actual expected)
  | actual == expected = pure expected
  | otherwise          = noRuleTo (Unify actual expected)

runUnification :: ( Eq usage
                  , Member (Exc (Some (Unify usage))) effects
                  )
               => Proof usage (Unify usage ': effects) a
               -> Proof usage effects a
runUnification = refine unification


data Unify usage result where
  Unify :: Type usage -> Type usage -> Unify usage (Type usage)
