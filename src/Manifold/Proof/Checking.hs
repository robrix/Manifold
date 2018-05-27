{-# LANGUAGE DataKinds, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Manifold.Proof.Checking where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Semiring (zero)
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
import Manifold.Type (Type, tvar, unitT, boolT, (.->), (.*))
import Manifold.Unification
import Manifold.Value.Intro

checkModule :: ( Eq usage
               , Members '[ Exc (Error (Annotated usage))
                          , Fresh
                          ] effects
               , Monoid usage
               )
            => Module Name (Term Name)
            -> Proof usage effects (Module (Annotated usage) (Term Name))
checkModule (Module name decls) = runContext (Module name <$> traverse checkDeclaration decls)


checkDeclaration :: ( Eq usage
                    , Members '[ Exc (Error (Annotated usage))
                               , Fresh
                               , Reader (Context (Annotated usage) (Type (Annotated usage)))
                               ] effects
                    , Monoid usage
                    )
                 => Declaration Name (Term Name)
                 -> Proof usage effects (Declaration (Annotated usage) (Term Name))
checkDeclaration (Declaration (name ::: ty) term) = do
  -- FIXME: extend the context while checking
  ty' <- runReader Intensional (runSubstitution (check term ty))
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
      => Term Name
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
      => Term Name
      -> Proof usage effects (Type (Annotated usage))
infer term = case unTerm term of
  Var name                                 -> do
    context <- askContext
    maybe (freeVariable name) (pure . constraintValue) (contextLookup name context)
  Intro i
    | Unit         <- i -> pure unitT
    | Bool _       <- i -> pure boolT
    -- | Abs var body <- i -> do
    --   ty' <- checkIsType ty
    --   let binding = Annotated var zero
    --   body' <- binding ::: ty' >- infer body
    --   pure (binding ::: ty' .-> body')
    | Pair a b              <- i -> (.*) <$> infer a <*> infer b
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
  _ -> noRuleToInferType term


runSubstitution :: Named var => Proof usage (State (Substitution (Type var)) ': effects) (Type var) -> Proof usage effects (Type var)
runSubstitution = fmap (uncurry (flip apply)) . runState mempty


askPurpose :: (Effectful m, Member (Reader Purpose) effects) => m effects Purpose
askPurpose = ask
