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
import Manifold.Type

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
unify t1 t2
  | t1 == t2  = pure t2
  | otherwise = case (unType t1, unType t2) of
    (Var n1, _)          -> n1 >-> t2 $> t2
    (_, Var n2)          -> n2 >-> t1 $> t1
    (Intro i1, Intro i2)
      | Unit    <- i1, Unit    <- i2 -> pure (tintro Unit)
      | Bool b1 <- i1, Bool b2 <- i2
      , b1 == b2                     -> pure (tintro (Bool b2))
      | Abs ((n1, u1) ::: t1) b1 <- i1
      , Abs ((n2, u2) ::: t2) b2 <- i2 -> do
        n' <- I <$> fresh
        t' <- unify t1 t2
        b' <- unify (apply (singletonSubst n1 (tvar n')) b1)
                    (apply (singletonSubst n2 (tvar n')) b2)
        pure (tintro (Abs ((n', u1 >< u2) ::: t') b'))
      | Pair a1 b1 <- i1, Pair a2 b2 <- i2 -> fmap tintro . Pair <$> unify a1 a2 <*> unify b1 b2
      | UnitT   <- i1, UnitT   <- i2 -> pure unitT
      | BoolT   <- i1, BoolT   <- i2 -> pure boolT
      | TypeT   <- i1, TypeT   <- i2 -> pure typeT
      | (n1, u1) ::: t1 :-> b1 <- i1
      , (n2, u2) ::: t2 :-> b2 <- i2 -> do
        n' <- I <$> fresh
        t' <- unify t1 t2
        b' <- unify (apply (singletonSubst n1 (tvar n')) b1)
                    (apply (singletonSubst n2 (tvar n')) b2)
        pure ((n', u1 >< u2) ::: t' .-> b')
      | a1 :* b1 <- i1, a2 :* b2 <- i2 -> (.*) <$> unify a1 a2 <*> unify b1 b2
    (Elim e1, Elim e2)
      | ExL a1      <- e1, ExL a2      <- e2 -> telim . ExL <$> unify a1 a2
      | ExR a1      <- e1, ExR a2      <- e2 -> telim . ExR <$> unify a1 a2
      | App f1 a1   <- e1, App f2 a2   <- e2 -> fmap telim . App <$> unify f1 f2 <*> unify a1 a2
      | If c1 t1 e1 <- e1, If c2 t2 e2 <- e2 -> fmap (fmap telim) . If <$> unify c1 c2 <*> unify t1 t2 <*> unify e1 e2
    _                                        -> cannotUnify t1 t2


(>->) :: Member (State (Substitution (Type usage))) effects => Name -> Type usage -> Proof usage effects ()
name >-> sub = modify' (<> singletonSubst name sub)
