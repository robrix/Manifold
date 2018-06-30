{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, KindSignatures, TypeApplications, TypeOperators #-}
module Manifold.Proof.Checking where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Internal hiding (apply)
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Foldable (foldl')
import Data.Semiring (Semiring(..), Unital(..), zero)
import Data.Semilattice.Lower
import Manifold.Constraint
import Manifold.Context
import Manifold.Declaration
import Manifold.Module as Module
import Manifold.Name
import Manifold.Name.Annotated
import Manifold.Pattern
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

checkModule :: ( Effects effects
               , Eq usage
               , Member (Exc (Error (Annotated usage))) effects
               , Member (Reader (ModuleTable Name (Term Name))) effects
               , Member (State (ModuleTable (Annotated usage) (Term Name))) effects
               , Monoid usage
               , Unital usage
               )
            => Module Name (Term Name)
            -> Proof usage effects (Module (Annotated usage) (Term Name))
checkModule m = lookupEvaluated (moduleName m) >>= maybe (cacheModule m) pure

cacheModule :: ( Effects effects
               , Eq usage
               , Member (Exc (Error (Annotated usage))) effects
               , Member (Reader (ModuleTable Name (Term Name))) effects
               , Member (State (ModuleTable (Annotated usage) (Term Name))) effects
               , Monoid usage
               , Unital usage
               )
            => Module Name (Term Name)
            -> Proof usage effects (Module (Annotated usage) (Term Name))
cacheModule (Module name imports decls) = do
  cacheEvaluated (Module name imports [])
  imports' <- traverse (\ name -> lookupUnevaluated name >>= maybe (unknownModule name) checkModule) imports
  decls' <- runFresh 0 (runContext (runIsType (foldr (>-) (foldr combine (pure []) decls) (imports' >>= moduleExports))))
  let m = Module name imports decls'
  m <$ cacheEvaluated m
  where combine decl rest = do
          decl' <- checkDeclaration decl
          foldr (>-) ((decl' :) <$> rest) (declarationSignatures decl')

lookupUnevaluated :: Member (Reader (ModuleTable Name (Term Name))) effects => Name -> Proof usage effects (Maybe (Module Name (Term Name)))
lookupUnevaluated name = asks (Module.lookup name)

lookupEvaluated :: Member (State (ModuleTable (Annotated usage) (Term Name))) effects => Name -> Proof usage effects (Maybe (Module (Annotated usage) (Term Name)))
lookupEvaluated name = gets (Module.lookup name)

cacheEvaluated :: Member (State (ModuleTable (Annotated usage) (Term Name))) effects => Module (Annotated usage) (Term Name) -> Proof usage effects ()
cacheEvaluated = modify' . insert


checkDeclaration :: ( Effects effects
                    , Eq usage
                    , Member (Exc (Error (Annotated usage))) effects
                    , Member Fresh effects
                    , Member (IsType usage) effects
                    , Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) effects
                    , Monoid usage
                    , Unital usage
                    )
                 => Declaration Name (Term Name)
                 -> Proof usage effects (Declaration (Annotated usage) (Term Name))
checkDeclaration (Binding (name ::: ty) term) = do
  ty' <- isType ty
  let annotated = Annotated name one
  ty'' <- annotated ::: ty' >- runSigma Intensional (runSubstitution (runUnify (runCheck (check term ty'))))
  pure (Binding (annotated ::: ty'') term)
checkDeclaration (Datatype (name ::: ty) constructors) = do
  ty' <- isType ty
  let annotated = Annotated name zero
  constructors' <- annotated ::: ty' >- runSigma Extensional (traverse checkConstructor constructors)
  pure (Datatype (annotated ::: ty') constructors')
  where checkConstructor (name ::: ty) = (Annotated name zero :::) <$> isType ty

runCheck :: ( Effects effects
            , Eq usage
            , Member (Exc (Error (Annotated usage))) effects
            , Member Fresh effects
            , Member (Reader usage) effects
            , Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) effects
            , Member (State (Substitution (Type (Annotated usage)))) effects
            , Member (Unify usage) effects
            , Monoid usage
            , Unital usage
            )
         => Proof usage (Check usage ': effects) a
         -> Proof usage effects a
runCheck = go . lowerEff
  where go (Return a) = pure a
        go (Effect (Check term expected) k) = runCheck $ case (unTerm term, unType expected) of
          (Value (Abs var body), IntroT (Annotated name pi ::: _S :-> _T))
            | _S == typeT -> do
              check term _T >>= Proof . k
            | otherwise -> do
              sigma <- ask
              _T' <- Annotated var (sigma >< pi) ::: _S >- check body _T
              Proof (k (Annotated name pi ::: _S .-> _T'))
          (Elim (Case subject matches), _) -> do
            subject' <- infer subject
            foldl' (checkMatch subject') (pure expected) matches >>= Proof . k
          _ -> do
            actual <- infer term
            unify actual expected >>= Proof . k
          where checkMatch subject expected (pattern, body) = do
                  expected' <- expected
                  checkPattern pattern subject (check body expected')
        go (Effect (Infer term) k) = runCheck $ case unTerm term of
          Var name -> lookupType name >>= Proof . k
          Value (Data name vs) -> do
            _C <- lookupType name
            checkFields _C vs >>= Proof . k
            where checkFields ty                                 []       = pure ty
                  checkFields (Type (IntroT (_ ::: ty :-> ret))) (v : vs) = check v ty >> checkFields ret vs
                  checkFields ty                                 _        = do
                    t1 <- freshName
                    t2 <- freshName
                    cannotUnify ty (Annotated t1 zero ::: ty .-> tvar t2)
          Elim (App f a) -> do
            n <- I <$> fresh
            t1 <- freshName
            t2 <- freshName
            _ <- check f (Annotated n zero ::: tvar t1 .-> tvar t2)
            _ <- check a (tvar t1)
            Proof (k (tvar t2))
          Elim (If c t e) -> do
            _ <- check c boolT
            t' <- infer t
            e' <- infer e
            unify t' e' >>= Proof . k
          _ -> noRuleToInferType term >>= Proof . k
        go (Other u k) = handle runCheck u (Proof . k)

checkPattern :: ( Eq usage
                , Member (Exc (Error (Annotated usage))) effects
                , Member Fresh effects
                , Member (Reader usage) effects
                , Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) effects
                , Member (State (Substitution (Type (Annotated usage)))) effects
                , Member (Unify usage) effects
                , Monoid usage
                )
             => Pattern Name
             -> Type (Annotated usage)
             -> Proof usage effects (Type (Annotated usage))
             -> Proof usage effects (Type (Annotated usage))
checkPattern (Pattern Wildcard)                    _       = id
checkPattern (Pattern (Variable name))             subject = (Annotated name zero ::: subject >-)
checkPattern (Pattern (Constructor name patterns)) subject = \ action -> do
  conTy <- lookupType name
  checkPatterns conTy patterns action
  where checkPatterns ty                                 []       = (unify ty subject >>)
        checkPatterns (Type (IntroT (_ ::: ty :-> ret))) (p : ps) = checkPattern p ty . checkPatterns ret ps
        checkPatterns ty                                 _        = const (cannotUnify ty subject)


runSubstitution :: Effects effects => Named var => Proof usage (State (Substitution (Type var)) ': effects) (Type var) -> Proof usage effects (Type var)
runSubstitution = fmap (uncurry apply) . runState lowerBound

runSigma :: (Effects effects, Monoid usage, Unital usage) => Purpose -> Proof usage (Reader usage ': effects) a -> Proof usage effects a
runSigma Extensional = runReader zero
runSigma Intensional = runReader one


check :: Member (Check usage) effects => Term Name -> Type (Annotated usage) -> Proof usage effects (Type (Annotated usage))
check term ty = send (Check term ty)

infer :: Member (Check usage) effects => Term Name -> Proof usage effects (Type (Annotated usage))
infer term = send (Infer term)

data Check usage (m :: * -> *) result where
  Check :: Term Name -> Type (Annotated usage) -> Check usage m (Type (Annotated usage))
  Infer :: Term Name                           -> Check usage m (Type (Annotated usage))


instance Effect (Check usage) where
  handleState c dist (Request (Check term ty) k) = Request (Check term ty) (dist . (<$ c) . k)
  handleState c dist (Request (Infer term)    k) = Request (Infer term)    (dist . (<$ c) . k)
