{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Manifold.Eval where

import qualified Data.Set as Set
import Manifold.Abstract.Env
import Manifold.Abstract.Evaluator
import Manifold.Abstract.Store
import Manifold.Abstract.Value as Value
import Manifold.Constraint
import Manifold.Name
import Manifold.Pattern
import Manifold.Term as Term
import Manifold.Term.Elim
import Manifold.Term.Intro

runEval :: ( Member (Allocator address value) sig
           , Member (Data value) sig
           , Member (Env address) sig
           , Member (Function value) sig
           , Member (Resumable (EnvError address)) sig
           , Member (Resumable (StoreError address value)) sig
           , Member (State (Store address value)) sig
           , Ord address
           , Ord value
           , Carrier sig m
           )
        => Evaluator address value (EvalC (Evaluator address value m)) a
        -> Evaluator address value m a
runEval = runEvalC . interpret . runEvaluator

newtype EvalC m a = EvalC { runEvalC :: m a }

instance ( Member (Allocator address value) sig
         , Member (Data value) sig
         , Member (Env address) sig
         , Member (Function value) sig
         , Member (Resumable (EnvError address)) sig
         , Member (Resumable (StoreError address value)) sig
         , Member (State (Store address value)) sig
         , Ord address
         , Ord value
         , Carrier sig m
         )
      => Carrier (Eval value :+: sig) (EvalC (Evaluator address value m)) where
  gen = EvalC . gen
  alg = algE \/ (EvalC . alg . handlePure runEvalC)
    where algE (Eval (Term term) k) = EvalC $ runEvalC . k =<< runEval (case term of
            Var name -> lookupEnv name >>= deref
            Intro i -> case i of
              Abs var body -> do
                let fvs = Set.delete (name var) (freeVariables body)
                env <- close fvs
                lambda (name var) (foldr (\ (v ::: addr) -> v .= addr) (eval body) env)
              Data c as -> traverse eval as >>= construct c
            Elim e -> case e of
              App f a -> do
                f' <- eval f
                a' <- eval a
                f' `apply` a'
              Case s bs -> do
                s' <- eval s
                match <- foldr (\ (pattern, branch) rest -> do
                  res <- match s' pattern (Just <$> eval branch)
                  maybe rest (pure . Just) res) (pure Nothing) bs
                maybe (error "non-exhaustive pattern match, should have been caught by typechecker") pure match)
          match _ (Pattern Wildcard) next = next
          match s (Pattern (Variable name)) next = do
            address <- alloc name
            assign address s
            name .= address $ next
          match s (Pattern (Constructor c' ps)) next = do
            (c, vs) <- deconstruct s
            if c == c' && length vs == length ps then
              foldr (uncurry match) next (zip vs ps)
            else
              pure Nothing


eval :: (Member (Eval value) sig, Carrier sig m) => Term Name -> Evaluator address value m value
eval = send . flip Eval gen

data Eval value m k
  = Eval (Term Name) (value -> k)
  deriving (Functor)


instance HFunctor (Eval value) where
  hfmap _ (Eval term k) = Eval term k
instance Effect (Eval value) where
  handle state handler (Eval term k) = Eval term (handler . (<$ state) . k)
