{-# LANGUAGE DataKinds, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Manifold.Proof.Checking where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Functor (($>))
import Data.Semiring (zero)
import Manifold.Constraint
import Manifold.Context
import Manifold.Declaration
import Manifold.Expr
import Manifold.Name
import Manifold.Name.Annotated
import Manifold.Proof
import Manifold.Proof.Formation
import Manifold.Purpose
import Manifold.Substitution
import Manifold.Term
import Manifold.Type
import Manifold.Unification


checkDeclaration :: ( Eq usage
                    , Members '[ Exc (Error (Annotated usage))
                               , Fresh
                               , Reader Purpose
                               , Reader (Context (Annotated usage) (Type (Annotated usage)))
                               , State (Substitution (Type (Annotated usage)))
                               ] effects
                    , Monoid usage
                    )
                 => Declaration Name Term
                 -> Proof usage effects (Declaration (Annotated usage) Term)
checkDeclaration (Declaration (name ::: ty) term) = do
  -- FIXME: extend the context while checking
  -- FIXME: use the Purpose
  ty' <- check term ty
  pure (Declaration (Annotated name zero ::: ty') term)


check :: ( Eq usage
         , Members '[ Exc (Error (Annotated usage))
                    , Fresh
                    , Reader Purpose
                    , Reader (Context (Annotated usage) (Type (Annotated usage)))
                    , State (Substitution (Type (Annotated usage)))
                    ] effects
         , Monoid usage
         )
      => Term
      -> Type Name
      -> Proof usage effects (Type (Annotated usage))
check term expected = do
  expected' <- checkIsType expected
  actual <- infer term
  unify actual expected'

infer :: ( Eq usage
         , Members '[ Exc (Error (Annotated usage))
                    , Fresh
                    , Reader Purpose
                    , Reader (Context (Annotated usage) (Type (Annotated usage)))
                    , State (Substitution (Type (Annotated usage)))
                    ] effects
         , Monoid usage
         )
      => Term
      -> Proof usage effects (Type (Annotated usage))
infer term = case unTerm term of
  Var name                                 -> do
    context <- askContext
    maybe (freeVariable name) (pure . constraintValue) (contextLookup name context)
  Intro i
    | Unit                  <- i -> pure unitT
    | Bool _                <- i -> pure boolT
    | Abs (var ::: ty) body <- i -> do
      ty' <- checkIsType ty
      let binding = Annotated var zero
      body' <- binding ::: ty' >- infer body
      pure (binding ::: ty' .-> body')
    | Pair a b              <- i -> (.*) <$> infer a <*> infer b
    | UnitT                 <- i -> pure typeT
    | BoolT                 <- i -> pure typeT
    | TypeT                 <- i -> pure typeT
    | var ::: ty :-> body   <- i -> do
      (ty' :: Type (Annotated usage)) <- checkIsType ty
      Annotated var (zero @usage) ::: ty' >- checkIsType (asType body) $> typeT
    | a :* b                <- i -> checkIsType (asType a) *> checkIsType (asType b) $> typeT
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
      _ <- check f (n ::: tvar t1 .-> tvar t2)
      _ <- check a (tvar t1)
      pure (tvar t2)
    | If c t e <- e -> do
      _ <- check c boolT
      t' <- infer t
      e' <- infer e
      unify t' e'


runSubstitution :: Named var => Proof usage (State (Substitution (Type var)) ': effects) a -> Proof usage effects (a, Substitution (Type var))
runSubstitution = runState mempty


askPurpose :: (Effectful m, Member (Reader Purpose) effects) => m effects Purpose
askPurpose = ask
