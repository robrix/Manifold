{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators #-}
module Manifold.Unification where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.State
import Data.Functor (($>))
import Data.Semiring (Semiring(..))
import Manifold.Name
import Manifold.Presyntax
import Manifold.Proof
import Manifold.Substitution

unify :: Member (Unify usage) effects => Type usage -> Type usage -> Proof usage effects (Type usage)
unify actual expected = send (Unify actual expected)

unification :: ( Eq usage
               , Members '[ Exc (Some (Unify usage))
                          , Fresh
                          , State (Substitution (Type usage))
                          , Unify usage
                          ] effects
               , Semiring usage
               )
            => Unify usage result
            -> Proof usage effects result
unification (Unify actual expected)
  | actual == expected = pure expected
  | otherwise          = Type <$> case (unType actual, unType expected) of
    (Var n1, t2)                                         -> n1 >-> Type t2 $> t2
    (t1, Var n2)                                         -> n2 >-> Type t1 $> t1
    (Unit, Unit)                                         -> pure Unit
    (UnitType, UnitType)                                 -> pure UnitType
    (BoolType, BoolType)                                 -> pure BoolType
    (T, T)                                               -> pure T
    (F, F)                                               -> pure F
    (TypeType, TypeType)                                 -> pure TypeType
    ((n1, u1) ::: t1 :-> b1, (n2, u2) ::: t2 :-> b2)     -> do
      n' <- I <$> fresh
      t' <- unify t1 t2
      b' <- unify (apply (singletonSubst n1 (Type (Var n'))) b1) (apply (singletonSubst n2 (Type (Var n'))) b2)
      pure ((n', u1 >< u2) ::: t' :-> b')
    (Abs ((n1, u1) ::: t1) b1, Abs ((n2, u2) ::: t2) b2) -> do
      n' <- I <$> fresh
      t' <- unify t1 t2
      b' <- unify (apply (singletonSubst n1 (Type (Var n'))) b1) (apply (singletonSubst n2 (Type (Var n'))) b2)
      pure (Abs ((n', u1 >< u2) ::: t') b')
    (App f1 a1, App f2 a2)                               -> App <$> unify f1 f2 <*> unify a1 a2
    (If c1 t1 e1, If c2 t2 e2)                           -> If <$> unify c1 c2 <*> unify t1 t2 <*> unify e1 e2
    (a1 :* b1, a2 :* b2)                                 -> (:*) <$> unify a1 a2 <*> unify b1 b2
    (Pair a1 b1, Pair a2 b2)                             -> Pair <$> unify a1 a2 <*> unify b1 b2
    (Nth i1 a1, Nth i2 a2) | i1 == i2                    -> Nth i2 <$> unify a1 a2
    (Ann a1 t1, Ann a2 t2)                               -> Ann <$> unify a1 a2 <*> unify t1 t2
    _                                                    -> noRuleTo (Unify actual expected)

runUnification :: ( Eq usage
                  , Members '[ Exc (Some (Unify usage))
                             , Fresh
                             ] effects
                  , Semiring usage
                  )
               => Proof usage (Unify usage ': State (Substitution (Type usage)) ': effects) (Type usage)
               -> Proof usage effects (Type usage)
runUnification = fmap (uncurry (flip apply)) . runState mempty . refine unification


data Unify usage result where
  Unify :: Type usage -> Type usage -> Unify usage (Type usage)


(>->) :: Member (State (Substitution (Type usage))) effects => Name -> Type usage -> Proof usage effects ()
name >-> sub = modify' (<> singletonSubst name sub)
