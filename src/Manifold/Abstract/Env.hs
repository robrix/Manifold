{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, GADTs, PolyKinds, StandaloneDeriving, TypeOperators #-}
module Manifold.Abstract.Env where

import qualified Data.Set as Set
import Manifold.Constraint
import Manifold.Context
import Manifold.Abstract.Evaluator
import Manifold.Name
import Manifold.Pretty

type Environment address = Context (Constraint Name address)


lookupEnv :: (Member (Env address) sig, Member (Resumable (EnvError address)) sig, Carrier sig m) => Name -> Evaluator address value m address
lookupEnv name = send (Lookup name gen) >>= maybe (throwResumable (FreeVariable name)) pure

close :: (Member (Env address) sig, Carrier sig carrier) => Set.Set Name -> Evaluator address value carrier (Environment address)
close = send . flip Close gen


(.=) :: (Member (Env address) sig, Carrier sig carrier) => Name -> address -> Evaluator address value carrier a -> Evaluator address value carrier a
name .= value = send . flip (Bind name value) gen

infixl 1 .=


data Env address m k
  = Lookup Name (Maybe address -> k)
  | forall a . Bind Name address (m a) (a -> k)
  | Close (Set.Set Name) (Environment address -> k)

deriving instance Functor (Env address m)

instance HFunctor (Env address) where
  hfmap _ (Lookup name k)      = Lookup name k
  hfmap f (Bind name addr m k) = Bind name addr (f m) k
  hfmap _ (Close addrs k)      = Close addrs k

instance Effect (Env address) where
  handle state handler (Lookup name k)      = Lookup name (handler . (<$ state) . k)
  handle state handler (Bind name addr m k) = Bind name addr (handler (m <$ state)) (handler . fmap k)
  handle state handler (Close fvs k)        = Close fvs (handler . (<$ state) . k)


data EnvError address result where
  FreeVariable :: Name -> EnvError address address

instance Pretty1 (EnvError address) where
  liftPrettyPrec _ d (FreeVariable name) = prettyParen (d > 0) $ prettyString "free variable:" <+> pretty name
