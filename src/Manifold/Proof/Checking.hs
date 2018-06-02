{-# LANGUAGE DataKinds, FlexibleContexts, TypeApplications, TypeOperators #-}
module Manifold.Proof.Checking where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Foldable (foldl')
import Data.Semiring (Semiring(..), Unital(..), zero)
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

checkModule :: ( Eq usage
               , Members '[ Exc (Error (Annotated usage))
                          , Reader (ModuleTable Name (Term Name))
                          , State (ModuleTable (Annotated usage) (Term Name))
                          ] effects
               , Monoid usage
               , Unital usage
               )
            => Module Name (Term Name)
            -> Proof usage effects (Module (Annotated usage) (Term Name))
checkModule m = lookupEvaluated (moduleName m) >>= maybe (cacheModule m) pure

cacheModule :: ( Eq usage
               , Members '[ Exc (Error (Annotated usage))
                          , Reader (ModuleTable Name (Term Name))
                          , State (ModuleTable (Annotated usage) (Term Name))
                          ] effects
               , Monoid usage
               , Unital usage
               )
            => Module Name (Term Name)
            -> Proof usage effects (Module (Annotated usage) (Term Name))
cacheModule (Module name imports decls) = do
  cacheEvaluated (Module name imports [])
  imports' <- traverse (\ name -> lookupUnevaluated name >>= maybe (unknownModule name) checkModule) imports
  decls' <- runFresh 0 (runContext (foldr (>-) (foldr combine (pure []) decls) (imports' >>= moduleExports)))
  let m = Module name imports decls'
  m <$ cacheEvaluated m
  where combine decl rest = do
          decl' <- checkDeclaration decl
          declarationSignature decl' >- (decl' :) <$> rest

lookupUnevaluated :: Member (Reader (ModuleTable Name (Term Name))) effects => Name -> Proof usage effects (Maybe (Module Name (Term Name)))
lookupUnevaluated name = asks (Module.lookup name)

lookupEvaluated :: Member (State (ModuleTable (Annotated usage) (Term Name))) effects => Name -> Proof usage effects (Maybe (Module (Annotated usage) (Term Name)))
lookupEvaluated name = gets (Module.lookup name)

cacheEvaluated :: Member (State (ModuleTable (Annotated usage) (Term Name))) effects => Module (Annotated usage) (Term Name) -> Proof usage effects ()
cacheEvaluated = modify' . insert


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
checkDeclaration (Binding (name ::: ty) term) = do
  ty' <- checkIsType ty
  let annotated = Annotated name one
  ty'' <- annotated ::: ty' >- runSigma Intensional (runSubstitution (check term ty'))
  pure (Binding (annotated ::: ty'') term)
checkDeclaration (Datatype (name ::: ty) constructors) = do
  ty' <- checkIsType ty
  let annotated = Annotated name zero
  constructors' <- annotated ::: ty' >- runSigma Extensional (traverse checkConstructor constructors)
  pure (Datatype (annotated ::: ty') constructors')
  where checkConstructor (name ::: ty) = (Annotated name zero :::) <$> checkIsType ty


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
  (Elim (Case subject matches), _) -> do
    subject' <- infer subject
    foldl' (checkMatch subject') (pure expected) matches
  _ -> do
    actual <- infer term
    unify actual expected
  where checkMatch subject expected (pattern, body) = do
          expected' <- expected
          checkPattern pattern subject (check body expected')

checkPattern :: ( Eq usage
                , Members '[ Exc (Error (Annotated usage))
                           , Fresh
                           , Reader usage
                           , Reader (Context (Annotated usage) (Type (Annotated usage)))
                           , State (Substitution (Type (Annotated usage)))
                           ] effects
                , Monoid usage
                )
             => Pattern
             -> Type (Annotated usage)
             -> Proof usage effects (Type (Annotated usage))
             -> Proof usage effects (Type (Annotated usage))
checkPattern Wildcard                    _       = id
checkPattern (Variable name)             subject = (Annotated name zero ::: subject >-)
checkPattern (Constructor name patterns) subject = \ action -> do
  context <- askContext
  conTy <- maybe (freeVariable name) (pure . constraintValue) (contextLookup name context)
  checkPatterns conTy patterns action
  where checkPatterns ty                                 []       = (unify ty subject >>)
        checkPatterns (Type (IntroT (_ ::: ty :-> ret))) (p : ps) = checkPattern p ty . checkPatterns ret ps
        checkPatterns ty                                 _        = const (cannotUnify ty subject)

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
  Var name -> do
    context <- askContext
    maybe (freeVariable name) (pure . constraintValue) (contextLookup name context)
  Value Unit       -> pure unitT
  Value (Bool _)   -> pure boolT
  Value (Pair a b) -> (.*) <$> infer a <*> infer b
  Elim (ExL a) -> do
    t1 <- freshName
    t2 <- freshName
    _ <- check a (tvar t1 .* tvar t2)
    pure (tvar t1)
  Elim (ExR a) -> do
    t1 <- freshName
    t2 <- freshName
    _ <- check a (tvar t1 .* tvar t2)
    pure (tvar t2)
  Elim (App f a) -> do
    n <- I <$> fresh
    t1 <- freshName
    t2 <- freshName
    _ <- check f (Annotated n zero ::: tvar t1 .-> tvar t2)
    _ <- check a (tvar t1)
    pure (tvar t2)
  Elim (If c t e) -> do
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
