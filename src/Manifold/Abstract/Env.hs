{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators #-}
module Manifold.Abstract.Env where

import qualified Data.Set as Set
import Manifold.Constraint
import Manifold.Context
import Manifold.Abstract.Evaluator
import Manifold.Name
import Manifold.Pretty

type Environment address = Context (Constraint Name address)


lookupEnv :: (Member (Env address) effects, Member (Resumable (EnvError address)) effects) => Name -> Evaluator address value effects address
lookupEnv name = send (Lookup name) >>= maybe (throwResumable (FreeVariable name)) pure

close :: Member (Env address) effects => Set.Set Name -> Evaluator address value effects (Environment address)
close = send . Close


(.=) :: Member (Env address) effects => Name -> address -> Evaluator address value effects a -> Evaluator address value effects a
name .= value = send . Bind name value . runEvaluator

infixl 1 .=


data Env address m result where
  Lookup :: Name                   -> Env address m (Maybe address)
  Bind   :: Name -> address -> m a -> Env address m a
  Close  :: Set.Set Name           -> Env address m (Environment address)

instance PureEffect (Env address)
instance Effect (Env address) where
  handleState state handler (Request (Lookup name) k) = Request (Lookup name) (handler . (<$ state) . k)
  handleState state handler (Request (Bind name addr m) k) = Request (Bind name addr (handler (m <$ state))) (handler . fmap k)
  handleState state handler (Request (Close fvs) k) = Request (Close fvs) (handler . (<$ state) . k)


data EnvError address result where
  FreeVariable :: Name -> EnvError address address

instance Pretty1 (EnvError address) where
  liftPrettyPrec _ d (FreeVariable name) = prettyParen (d > 0) $ prettyString "free variable:" <+> pretty name
