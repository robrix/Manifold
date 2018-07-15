{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, KindSignatures, TypeOperators #-}
module Manifold.Eval where

import Control.Monad.Effect.Internal hiding (apply)
import Manifold.Abstract.Address
import Manifold.Abstract.Env
import Manifold.Abstract.Evaluator
import Manifold.Abstract.Store
import Manifold.Abstract.Value as Value
import Manifold.Constraint
import Manifold.Context
import Manifold.Name
import Manifold.Pattern
import Manifold.Term as Term
import Manifold.Term.Elim
import Manifold.Term.Intro

runEval :: ( Address address (Eval value ': effects)
           , Effects effects
           , Member (Reader (Env address)) effects
           , Member (Resumable (StoreError address value)) effects
           , Member (State (Store address value)) effects
           , Ord value
           , Value address value (Eval value ': effects)
           )
        => Evaluator address value (Eval value ': effects) a
        -> Evaluator address value effects a
runEval = go . lowerEff
  where go (Return a) = pure a
        go (Effect (Eval (Term term)) k) = runEval $ Evaluator . k =<< case term of
          Var name -> do
            address <- fmap constraintValue . contextLookup name <$> askEnv
            maybe (error "free variable, should have been caught by typechecker") deref address
          Intro i -> case i of
            Abs var body -> lambda (name var) body
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
              maybe (error "non-exhaustive pattern match, should have been caught by typechecker") pure match
          where match _ (Pattern Wildcard) next = next
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
        go (Other u k) = liftHandler runEval u (Evaluator . k)


eval :: Member (Eval value) effects => Term Name -> Evaluator address value effects value
eval = send . Eval

data Eval value (m :: * -> *) result where
  Eval :: Term Name -> Eval value m value


instance Effect (Eval value) where
  handleState c dist (Request (Eval term) k) = Request (Eval term) (dist . (<$ c) . k)
