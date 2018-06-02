{-# LANGUAGE DataKinds, FlexibleContexts #-}
module Manifold.Unification where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.State
import Data.Functor (($>))
import Manifold.Constraint
import Manifold.Context
import Manifold.Name
import Manifold.Proof
import Manifold.Substitution
import Manifold.Term.Elim
import Manifold.Type
import Manifold.Type.Intro
import Manifold.Value.Intro

unify :: ( Eq var
         , Members '[ Exc (Error var)
                    , Fresh
                    , State (Substitution (Type var))
                    ] effects
         , Member (Reader (Context var (Type var))) effects
         , Named var
         )
      => Type var
      -> Type var
      -> Proof usage effects (Type var)
unify t1 t2
  | t1 == t2  = pure t2
  | otherwise = case (unType t1, unType t2) of
    (Var n1, _)          -> n1 >-> t2 $> t2
    (_, Var n2)          -> n2 >-> t1 $> t1
    (Intro i1, Intro i2)
      | Unit    <- i1, Unit    <- i2 -> pure (tintro Unit)
      | Bool b1 <- i1, Bool b2 <- i2
      , b1 == b2                     -> pure (tintro (Bool b2))
      | Abs (v1 ::: t1) b1 <- i1
      , Abs (v2 ::: t2) b2 <- i2 -> do
        n' <- I <$> fresh
        t' <- unify t1 t2
        b' <- unify (apply (singletonSubst (name v1) (tvar n')) b1)
                    (apply (singletonSubst (name v2) (tvar n')) b2)
        pure (tintro (Abs (setName n' v1 ::: t') b'))
      | Pair a1 b1 <- i1, Pair a2 b2 <- i2 -> fmap tintro . Pair <$> unify a1 a2 <*> unify b1 b2
      | Data n1 ts1 <- i1
      , Data n2 ts2 <- i2
      , n1 == n2
      , length ts1 == length ts2 -> tintro . Data n2 <$> sequenceA (zipWith unify ts1 ts2)
    (IntroT i1, IntroT i2)
      | TypeT   <- i1, TypeT   <- i2 -> pure typeT
      | TypeC n1 ts1 <- i1
      , TypeC n2 ts2 <- i2
      , n1 == n2
      , length ts1 == length ts2 -> typeC n2 <$> sequenceA (zipWith unify ts1 ts2)
      | v1 ::: t1 :-> b1 <- i1
      , v2 ::: t2 :-> b2 <- i2 -> do
        n' <- I <$> fresh
        t' <- unify t1 t2
        b' <- unify (apply (singletonSubst (name v1) (tvar n')) b1)
                    (apply (singletonSubst (name v2) (tvar n')) b2)
        pure (setName n' v1 ::: t' .-> b')
      | a1 :* b1 <- i1, a2 :* b2 <- i2 -> (.*) <$> unify a1 a2 <*> unify b1 b2
    (Elim e1, Elim e2)
      | ExL a1      <- e1, ExL a2      <- e2 -> telim . ExL <$> unify a1 a2
      | ExR a1      <- e1, ExR a2      <- e2 -> telim . ExR <$> unify a1 a2
      | App f1 a1   <- e1, App f2 a2   <- e2 -> fmap telim . App <$> unify f1 f2 <*> unify a1 a2
      | If c1 t1 e1 <- e1, If c2 t2 e2 <- e2 -> fmap (fmap telim) . If <$> unify c1 c2 <*> unify t1 t2 <*> unify e1 e2
    _                                        -> cannotUnify t1 t2


(>->) :: (Member (State (Substitution (Type var))) effects, Named var) => Name -> Type var -> Proof usage effects ()
name >-> sub = modify' (<> singletonSubst name sub)
