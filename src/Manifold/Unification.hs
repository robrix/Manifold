{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, KindSignatures, TypeOperators #-}
module Manifold.Unification where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Internal hiding (apply)
import Control.Monad.Effect.State
import Manifold.Constraint
import Manifold.Context
import Manifold.Name
import Manifold.Name.Annotated
import Manifold.Proof
import Manifold.Substitution
import Manifold.Term.Elim
import Manifold.Type
import Manifold.Type.Intro
import Manifold.Value.Intro

runUnify :: ( Effects effects
            , Eq usage
            , Member (Exc (Error (Annotated usage))) effects
            , Member Fresh effects
            , Member (Reader (Context (Annotated usage) (Type (Annotated usage)))) effects
            , Member (State (Substitution (Type (Annotated usage)))) effects
            )
         => Proof usage (Unify usage ': effects) a
         -> Proof usage effects a
runUnify = go . lowerEff
  where go (Return a) = pure a
        go (Effect (Unify t1 t2) k)
          | t1 == t2  = runUnify $ Proof (k t2)
          | otherwise = runUnify $ case (unType t1, unType t2) of
            (Var n1, _)          -> n1 >-> t2 *> Proof (k t2)
            (_, Var n2)          -> n2 >-> t1 *> Proof (k t1)
            (Intro i1, Intro i2)
              | Abs (v1 ::: t1) b1 <- i1
              , Abs (v2 ::: t2) b2 <- i2 -> do
                n' <- I <$> fresh
                t' <- unify t1 t2
                b' <- unify (apply (singletonSubst (name v1) (tvar n')) b1)
                            (apply (singletonSubst (name v2) (tvar n')) b2)
                Proof (k (tintro (Abs (setName n' v1 ::: t') b')))
              | Data n1 ts1 <- i1
              , Data n2 ts2 <- i2
              , n1 == n2
              , length ts1 == length ts2 -> tintro . Data n2 <$> sequenceA (zipWith unify ts1 ts2) >>= Proof . k
            (IntroT i1, IntroT i2)
              | TypeT <- i1, TypeT <- i2 -> Proof (k typeT)
              | TypeC n1 ts1 <- i1
              , TypeC n2 ts2 <- i2
              , n1 == n2
              , length ts1 == length ts2 -> typeC n2 <$> sequenceA (zipWith unify ts1 ts2) >>= Proof . k
              | v1 ::: t1 :-> b1 <- i1
              , v2 ::: t2 :-> b2 <- i2 -> do
                n' <- I <$> fresh
                t' <- unify t1 t2
                b' <- unify (apply (singletonSubst (name v1) (tvar n')) b1)
                            (apply (singletonSubst (name v2) (tvar n')) b2)
                Proof (k (setName n' v1 ::: t' .-> b'))
              | a1 :* b1 <- i1, a2 :* b2 <- i2 -> (.*) <$> unify a1 a2 <*> unify b1 b2 >>= Proof . k
            (Elim e1, Elim e2)
              | App f1 a1   <- e1, App f2 a2   <- e2 -> fmap telim . App <$> unify f1 f2 <*> unify a1 a2 >>= Proof . k
              | If c1 t1 e1 <- e1, If c2 t2 e2 <- e2 -> fmap (fmap telim) . If <$> unify c1 c2 <*> unify t1 t2 <*> unify e1 e2 >>= Proof . k
            _                                        -> cannotUnify t1 t2
        go (Other u k) = handle runUnify u (Proof . k)


(>->) :: (Member (State (Substitution (Type (Annotated usage)))) effects, Named (Annotated usage)) => Name -> Type (Annotated usage) -> Proof usage effects ()
name >-> sub = modify' (<> singletonSubst name sub)


unify :: Member (Unify usage) effects => Type (Annotated usage) -> Type (Annotated usage) -> Proof usage effects (Type (Annotated usage))
unify t1 t2 = send (Unify t1 t2)

data Unify usage (m :: * -> *) result where
  Unify :: Type (Annotated usage) -> Type (Annotated usage) -> Unify usage m (Type (Annotated usage))

instance Effect (Unify usage) where
  handleState c dist (Request (Unify t1 t2) k) = Request (Unify t1 t2) (dist . (<$ c) . k)
