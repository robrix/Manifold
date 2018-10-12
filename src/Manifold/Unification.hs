{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Manifold.Unification where

import Control.Effect
import Manifold.Constraint
import Manifold.Context
import Manifold.Name
import Manifold.Name.Annotated
import Manifold.Proof
import Manifold.Substitution
import Manifold.Term.Elim
import Manifold.Term.Intro
import Manifold.Type
import Manifold.Type.Intro

runUnify :: ( Eq usage
            , Member (Error (ProofError (Annotated usage))) sig
            , Member Fresh sig
            , Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) sig
            , Member (State (Substitution (Type (Annotated usage)))) sig
            , Carrier sig m
            )
         => Proof usage (UnifyC usage (Proof usage m)) a
         -> Proof usage m a
runUnify = runUnifyC . interpret . runProof

newtype UnifyC usage m a = UnifyC { runUnifyC :: m a }

instance ( Eq usage
         , Member (Error (ProofError (Annotated usage))) sig
         , Member Fresh sig
         , Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) sig
         , Member (State (Substitution (Type (Annotated usage)))) sig
         , Carrier sig m
         )
      => Carrier (Unify usage :+: sig) (UnifyC usage (Proof usage m)) where
  gen = UnifyC . gen
  alg = algU \/ (UnifyC . alg . handlePure runUnifyC)
    where algU (Unify t1 t2 k)
            | t1 == t2  = k t2
            | otherwise = UnifyC $ case (unType t1, unType t2) of
              (Var n1, _)          -> n1 >-> t2 *> runUnifyC (k t2)
              (_, Var n2)          -> n2 >-> t1 *> runUnifyC (k t1)
              (Intro i1, Intro i2)
                | Abs (v1 ::: t1) b1 <- i1
                , Abs (v2 ::: t2) b2 <- i2 -> do
                  n' <- I <$> fresh
                  t' <- runUnify (unify t1 t2)
                  b' <- runUnify (unify (apply (singletonSubst (name v1) (tvar n')) b1)
                                        (apply (singletonSubst (name v2) (tvar n')) b2))
                  runUnifyC (k (tintro (Abs (setName n' v1 ::: t') b')))
                | Data n1 ts1 <- i1
                , Data n2 ts2 <- i2
                , n1 == n2
                , length ts1 == length ts2 -> runUnify (tintro . Data n2 <$> sequenceA (zipWith unify ts1 ts2)) >>= runUnifyC . k
              (IntroT i1, IntroT i2)
                | TypeT <- i1, TypeT <- i2 -> runUnifyC (k typeT)
                | TypeC n1 ts1 <- i1
                , TypeC n2 ts2 <- i2
                , n1 == n2
                , length ts1 == length ts2 -> runUnify (typeC n2 <$> sequenceA (zipWith unify ts1 ts2)) >>= runUnifyC . k
                | v1 ::: t1 :-> b1 <- i1
                , v2 ::: t2 :-> b2 <- i2 -> do
                  n' <- I <$> fresh
                  t' <- runUnify (unify t1 t2)
                  b' <- runUnify (unify (apply (singletonSubst (name v1) (tvar n')) b1)
                                        (apply (singletonSubst (name v2) (tvar n')) b2))
                  runUnifyC (k (setName n' v1 ::: t' .-> b'))
                | a1 :* b1 <- i1, a2 :* b2 <- i2 -> runUnify ((.*) <$> unify a1 a2 <*> unify b1 b2) >>= runUnifyC . k
              (Elim e1, Elim e2)
                | App f1 a1   <- e1, App f2 a2   <- e2 -> runUnify (fmap telim . App <$> unify f1 f2 <*> unify a1 a2) >>= runUnifyC . k
              _                                        -> cannotUnify t1 t2


(>->) :: (Member (State (Substitution (Type (Annotated usage)))) sig, Carrier sig m) => Name -> Type (Annotated usage) -> Proof usage m ()
name >-> sub = modify (<> singletonSubst name sub)


unify :: (Member (Unify usage) sig, Carrier sig m) => Type (Annotated usage) -> Type (Annotated usage) -> Proof usage m (Type (Annotated usage))
unify t1 t2 = send (Unify t1 t2 gen)

data Unify usage m k
  = Unify (Type (Annotated usage)) (Type (Annotated usage)) (Type (Annotated usage) -> k)
  deriving (Functor)

instance HFunctor (Unify usage) where
  hfmap _ (Unify t1 t2 k) = Unify t1 t2 k

instance Effect (Unify usage) where
  handle state handler (Unify t1 t2 k) = Unify t1 t2 (handler . (<$ state) . k)
