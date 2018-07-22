{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators #-}
module Manifold.Abstract.Env where

import Manifold.Constraint
import Manifold.Context
import Manifold.Abstract.Evaluator
import Manifold.Name
import Manifold.Pretty
import Data.Semilattice.Lower

type Environment address = Context (Constraint Name address)


askEnv :: Member (Reader (Environment address)) effects => Evaluator address value effects (Environment address)
askEnv = ask

lookupEnv :: (Member (Reader (Environment address)) effects, Member (Resumable (EnvError address)) effects) => Name -> Evaluator address value effects address
lookupEnv name = askEnv >>= maybe (throwResumable (FreeVariable name)) (pure . constraintValue) . contextLookup name


runEnv :: Effects effects => Evaluator address value (Reader (Environment address) ': effects) a -> Evaluator address value effects a
runEnv = runReader lowerBound


(.=) :: Member (Reader (Environment address)) effects => Name -> address -> Evaluator address value effects a -> Evaluator address value effects a
name .= value = local (|> (name ::: value))

infixl 1 .=


data Env address m result where
  Lookup :: Name                   -> Env address m address
  Bind   :: Name -> address -> m a -> Env address m a

instance PureEffect (Env address)
instance Effect (Env address) where
  handleState state handler (Request (Lookup name) k) = Request (Lookup name) (handler . (<$ state) . k)
  handleState state handler (Request (Bind name addr m) k) = Request (Bind name addr (handler (m <$ state))) (handler . fmap k)


data EnvError address result where
  FreeVariable :: Name -> EnvError address address

instance Pretty1 (EnvError address) where
  liftPrettyPrec _ d (FreeVariable name) = prettyParen (d > 0) $ prettyString "free variable:" <+> pretty name
