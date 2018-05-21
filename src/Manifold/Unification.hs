{-# LANGUAGE DataKinds, FlexibleContexts #-}
module Manifold.Unification where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.State
import Data.Functor (($>))
import Data.Semiring (Semiring(..))
import Manifold.Expr
import Manifold.Name
import Manifold.Proof
import Manifold.Substitution

unify :: ( Eq usage
         , Members '[ Exc (Error usage)
                    , Fresh
                    , State (Substitution (Type usage))
                    ] effects
         , Semiring usage
         )
      => Type usage
      -> Type usage
      -> Proof usage effects (Type usage)
unify actual expected
  | actual == expected = pure expected
  | otherwise          = Type <$> case (unType actual, unType expected) of
    (Var n1, t2)                                         -> n1 >-> Type t2 $> t2
    (t1, Var n2)                                         -> n2 >-> Type t1 $> t1
    (Unit, Unit)                                         -> pure Unit
    (UnitT, UnitT)                                       -> pure UnitT
    (BoolT, BoolT)                                       -> pure BoolT
    (Bool b1, Bool b2) | b1 == b2                        -> pure (Bool b2)
    (TypeT, TypeT)                                       -> pure TypeT
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
    (ExL a1, ExL a2)                                     -> ExL <$> unify a1 a2
    (ExR a1, ExR a2)                                     -> ExR <$> unify a1 a2
    _                                                    -> cannotUnify actual expected


(>->) :: Member (State (Substitution (Type usage))) effects => Name -> Type usage -> Proof usage effects ()
name >-> sub = modify' (<> singletonSubst name sub)
