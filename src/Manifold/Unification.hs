{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators #-}
module Manifold.Unification where

import Control.Monad.Effect
import Control.Monad.Effect.State
import Data.Functor (($>))
import Manifold.Name
import Manifold.Presyntax
import Manifold.Proof
import Manifold.Substitution

unify :: Member (Unify usage) effects => Type usage -> Type usage -> Proof usage effects (Type usage)
unify actual expected = send (Unify actual expected)

unification :: ( Eq usage
               , Members '[ Exc (Some (Unify usage))
                          , State (Substitution (Type usage))
                          , Unify usage
                          ] effects
               )
            => Unify usage result
            -> Proof usage effects result
unification (Unify actual expected)
  | actual == expected = pure expected
  | otherwise          = case (unType actual, unType expected) of
    (Var n1, _) -> n1 >-> expected
    (_, Var n2) -> n2 >-> actual
    _ -> noRuleTo (Unify actual expected)

runUnification :: ( Eq usage
                  , Member (Exc (Some (Unify usage))) effects
                  )
               => Proof usage (Unify usage ': State (Substitution (Type usage)) ': effects) (Type usage)
               -> Proof usage effects (Type usage)
runUnification = fmap (uncurry (flip apply)) . runState mempty . refine unification


data Unify usage result where
  Unify :: Type usage -> Type usage -> Unify usage (Type usage)


(>->) :: Member (State (Substitution (Type usage))) effects => Name -> Type usage -> Proof usage effects (Type usage)
name >-> sub = modify' (<> singletonSubst name sub) $> sub
