{-# LANGUAGE DataKinds, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Manifold.Proof.Checking where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Semiring (Semiring(..), Unital(..), zero)
import Manifold.Constraint
import Manifold.Context
import Manifold.Declaration
import Manifold.Module
import Manifold.Name
import Manifold.Name.Annotated
import Manifold.Proof
import Manifold.Proof.Formation
import Manifold.Purpose
import Manifold.Substitution
import Manifold.Term
import Manifold.Term.Elim
import Manifold.Type hiding (Var, Elim)
import Manifold.Type.Intro
import Manifold.Unification
import Manifold.Value.Intro

checkModule :: ( Eq usage
               , Members '[ Exc (Error (Annotated usage))
                          , Fresh
                          ] effects
               , Monoid usage
               , Unital usage
               )
            => Module Name (Term Name)
            -> Proof usage effects (Module (Annotated usage) (Term Name))
checkModule (Module name decls) = runContext (Module name <$> traverse checkDeclaration decls)
-- FIXME: extend the context while checking


checkDeclaration :: ( Eq usage
                    , Members '[ Exc (Error (Annotated usage))
                               , Fresh
                               , Reader (Context (Annotated usage) (Type (Annotated usage)))
                               ] effects
                    , Monoid usage
                    , Unital usage
                    )
                 => Declaration Name (Term Name)
                 -> Proof usage effects (Declaration (Annotated usage) (Term Name))
checkDeclaration (Declaration (name ::: ty) term) = do
  ty' <- checkIsType ty
  ty'' <- runSigma Intensional (runSubstitution (check term ty'))
  pure (Declaration (Annotated name zero ::: ty'') term)


check :: ( Eq usage
         , Members '[ Exc (Error (Annotated usage))
                    , Fresh
                    , Reader usage
                    , Reader (Context (Annotated usage) (Type (Annotated usage)))
                    , State (Substitution (Type (Annotated usage)))
                    ] effects
         , Monoid usage
         , Unital usage
         )
      => Term Name
      -> Type (Annotated usage)
      -> Proof usage effects (Type (Annotated usage))
check term expected = case (unTerm term, unType expected) of
  (Value (Abs var body), IntroT (Annotated name pi ::: _S :-> _T)) -> do
    sigma <- ask
    _T' <- Annotated var (sigma >< pi) ::: _S >- check body _T
    pure (Annotated name pi ::: _S .-> _T')
  _ -> do
    actual <- infer term
    unify actual expected

infer :: ( Eq usage
         , Members '[ Exc (Error (Annotated usage))
                    , Fresh
                    , Reader usage
                    , Reader (Context (Annotated usage) (Type (Annotated usage)))
                    , State (Substitution (Type (Annotated usage)))
                    ] effects
         , Monoid usage
         , Unital usage
         )
      => Term Name
      -> Proof usage effects (Type (Annotated usage))
infer term = case unTerm term of
  Var name                                 -> do
    context <- askContext
    maybe (freeVariable name) (pure . constraintValue) (contextLookup name context)
  Value i
    | Unit     <- i -> pure unitT
    | Bool _   <- i -> pure boolT
    | Pair a b <- i -> (.*) <$> infer a <*> infer b
  Elim e
    | ExL a    <- e -> do
      t1 <- freshName
      t2 <- freshName
      _ <- check a (tvar t1 .* tvar t2)
      pure (tvar t1)
    | ExR a    <- e -> do
      t1 <- freshName
      t2 <- freshName
      _ <- check a (tvar t1 .* tvar t2)
      pure (tvar t2)
    | App f a  <- e -> do
      n <- I <$> fresh
      t1 <- freshName
      t2 <- freshName
      _ <- check f (Annotated n zero ::: tvar t1 .-> tvar t2)
      _ <- check a (tvar t1)
      pure (tvar t2)
    | If c t e <- e -> do
      _ <- check c boolT
      t' <- infer t
      e' <- infer e
      unify t' e'
  _ -> noRuleToInferType term


runSubstitution :: Named var => Proof usage (State (Substitution (Type var)) ': effects) (Type var) -> Proof usage effects (Type var)
runSubstitution = fmap (uncurry (flip apply)) . runState mempty

runSigma :: (Monoid usage, Unital usage) => Purpose -> Proof usage (Reader usage ': effects) a -> Proof usage effects a
runSigma Extensional = runReader zero
runSigma Intensional = runReader one

askPurpose :: (Effectful m, Member (Reader Purpose) effects) => m effects Purpose
askPurpose = ask
