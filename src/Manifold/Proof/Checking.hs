{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeApplications, TypeOperators, UndecidableInstances #-}
module Manifold.Proof.Checking where

import Control.Effect
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
import Manifold.Term.Intro
import Manifold.Type hiding (Var, Elim, Intro)
import Manifold.Type.Intro
import Manifold.Unification

checkModule :: ( Carrier sig m
               , Effect sig
               , Eq usage
               , Member (Error (ProofError (Annotated usage))) sig
               , Member (Reader (ModuleTable Name (Term Name))) sig
               , Member (State (ModuleTable (Annotated usage) (Term Name))) sig
               , Monoid usage
               , Unital usage
               )
            => Module Name (Term Name)
            -> Proof usage m (Module (Annotated usage) (Term Name))
checkModule m = lookupEvaluated (moduleName m) >>= maybe (cacheModule m) pure

cacheModule :: ( Carrier sig m
               , Effect sig
               , Eq usage
               , Member (Error (ProofError (Annotated usage))) sig
               , Member (Reader (ModuleTable Name (Term Name))) sig
               , Member (State (ModuleTable (Annotated usage) (Term Name))) sig
               , Monoid usage
               , Unital usage
               )
            => Module Name (Term Name)
            -> Proof usage m (Module (Annotated usage) (Term Name))
cacheModule (Module name imports decls) = do
  cacheEvaluated (Module name imports [])
  imports' <- traverse (\ name -> lookupUnevaluated name >>= maybe (unknownModule name) checkModule) imports
  decls' <- runFresh (runProof (runContext (runIsType (foldr (>-) (foldr combine (pure []) decls) (imports' >>= moduleExports)))))
  let m = Module name imports decls'
  m <$ cacheEvaluated m
  where combine decl rest = do
          decl' <- checkDeclaration decl
          foldr (>-) ((decl' :) <$> rest) (declarationSignatures decl')

lookupUnevaluated :: (Member (Reader (ModuleTable Name (Term Name))) sig, Carrier sig m) => Name -> Proof usage m (Maybe (Module Name (Term Name)))
lookupUnevaluated name = asks (Module.lookup name)

lookupEvaluated :: (Member (State (ModuleTable (Annotated usage) (Term Name))) sig, Carrier sig m) => Name -> Proof usage m (Maybe (Module (Annotated usage) (Term Name)))
lookupEvaluated name = gets (Module.lookup name)

cacheEvaluated :: (Member (State (ModuleTable (Annotated usage) (Term Name))) sig, Carrier sig m) => Module (Annotated usage) (Term Name) -> Proof usage m ()
cacheEvaluated = modify . insert


checkDeclaration :: ( Carrier sig m
                    , Effect sig
                    , Eq usage
                    , Member (Error (ProofError (Annotated usage))) sig
                    , Member Fresh sig
                    , Member (IsType usage) sig
                    , Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) sig
                    , Monoid usage
                    , Unital usage
                    )
                 => Declaration Name (Term Name)
                 -> Proof usage m (Declaration (Annotated usage) (Term Name))
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

runCheck :: ( Eq usage
            , Member (Error (ProofError (Annotated usage))) sig
            , Member Fresh sig
            , Member (Reader usage) sig
            , Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) sig
            , Member (State (Substitution (Type (Annotated usage)))) sig
            , Member (Unify usage) sig
            , Monoid usage
            , Carrier sig m
            , Unital usage
            )
         => Proof usage (CheckC usage (Proof usage m)) a
         -> Proof usage m a
runCheck = runCheckC . interpret . runProof

newtype CheckC usage m a = CheckC { runCheckC :: m a }

instance ( Eq usage
         , Member (Error (ProofError (Annotated usage))) sig
         , Member Fresh sig
         , Member (Reader usage) sig
         , Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) sig
         , Member (State (Substitution (Type (Annotated usage)))) sig
         , Member (Unify usage) sig
         , Monoid usage
         , Carrier sig m
         , Unital usage
         )
      => Carrier (Check usage :+: sig) (CheckC usage (Proof usage m)) where
  gen = CheckC . gen
  alg = algC \/ (CheckC . alg . handlePure runCheckC)
    where algC (Check term expected k) = CheckC $ case (unTerm term, unType expected) of
            (Intro (Abs var body), IntroT (Annotated name pi ::: _S :-> _T))
              | _S == typeT -> do
                runCheck (check term _T) >>= runCheckC . k
              | otherwise -> do
                sigma <- ask
                _T' <- Annotated var (sigma >< pi) ::: _S >- runCheck (check body _T)
                runCheckC (k (Annotated name pi ::: _S .-> _T'))
            (Elim (Case subject matches), _) -> do
              subject' <- runCheck (infer subject)
              foldl' (checkMatch subject') (pure expected) matches >>= runCheckC . k
            _ -> do
              actual <- runCheck (infer term)
              unify actual expected >>= runCheckC . k
            where checkMatch subject expected (pattern, body) = do
                    expected' <- expected
                    checkPattern pattern subject (runCheck (check body expected'))
          algC (Infer term k) = CheckC $ case unTerm term of
            Var name -> lookupType name >>= runCheckC . k
            Intro (Data name vs) -> do
              _C <- lookupType name
              checkFields _C vs >>= runCheckC . k
              where checkFields ty                                 []       = pure ty
                    checkFields (Type (IntroT (_ ::: ty :-> ret))) (v : vs) = runCheck (check v ty) >> checkFields ret vs
                    checkFields ty                                 _        = do
                      t1 <- freshName
                      t2 <- freshName
                      cannotUnify ty (Annotated t1 zero ::: ty .-> tvar t2)
            Elim (App f a) -> do
              n <- I <$> fresh
              t1 <- freshName
              t2 <- freshName
              _ <- runCheck (check f (Annotated n zero ::: tvar t1 .-> tvar t2))
              _ <- runCheck (check a (tvar t1))
              runCheckC (k (tvar t2))
            _ -> noRuleToInferType term >>= runCheckC . k

checkPattern :: ( Eq usage
                , Member (Error (ProofError (Annotated usage))) sig
                , Member Fresh sig
                , Member (Reader usage) sig
                , Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) sig
                , Member (State (Substitution (Type (Annotated usage)))) sig
                , Member (Unify usage) sig
                , Carrier sig m
                , Monoid usage
                )
             => Pattern Name
             -> Type (Annotated usage)
             -> Proof usage m (Type (Annotated usage))
             -> Proof usage m (Type (Annotated usage))
checkPattern (Pattern Wildcard)                    _       = id
checkPattern (Pattern (Variable name))             subject = (Annotated name zero ::: subject >-)
checkPattern (Pattern (Constructor name patterns)) subject = \ action -> do
  conTy <- lookupType name
  checkPatterns conTy patterns action
  where checkPatterns ty                                 []       = (unify ty subject >>)
        checkPatterns (Type (IntroT (_ ::: ty :-> ret))) (p : ps) = checkPattern p ty . checkPatterns ret ps
        checkPatterns ty                                 _        = const (cannotUnify ty subject)


runSubstitution :: (Carrier sig m, Effect sig) => Named var => Proof usage (StateC (Substitution (Type var)) (Proof usage m)) (Type var) -> Proof usage m (Type var)
runSubstitution = fmap (uncurry apply) . runState lowerBound . runProof

runSigma :: (Carrier sig m, Monoid usage, Unital usage) => Purpose -> Proof usage (ReaderC usage (Proof usage m)) a -> Proof usage m a
runSigma Extensional = runReader zero . runProof
runSigma Intensional = runReader one  . runProof


check :: (Member (Check usage) sig, Carrier sig m) => Term Name -> Type (Annotated usage) -> Proof usage m (Type (Annotated usage))
check term ty = send (Check term ty gen)

infer :: (Member (Check usage) sig, Carrier sig m) => Term Name -> Proof usage m (Type (Annotated usage))
infer term = send (Infer term gen)

data Check usage m k
  = Check (Term Name) (Type (Annotated usage)) (Type (Annotated usage) -> k)
  | Infer (Term Name)                          (Type (Annotated usage) -> k)
  deriving (Functor)

instance HFunctor (Check usage) where
  hfmap _ (Check tm ty k) = Check tm ty k
  hfmap _ (Infer tm k) = Infer tm k

instance Effect (Check usage) where
  handle state handler (Check term ty k) = Check term ty (handler . (<$ state) . k)
  handle state handler (Infer term    k) = Infer term    (handler . (<$ state) . k)
