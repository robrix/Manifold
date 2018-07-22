{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, TypeOperators #-}
module Manifold.Abstract.Address.Precise where

import Data.Monoid (Last(..))
import qualified Data.Set as Set
import Manifold.Abstract.Env (Env(..), Environment)
import Manifold.Abstract.Evaluator
import Manifold.Abstract.Store
import Manifold.Constraint
import Manifold.Context
import Manifold.Name
import Manifold.Pretty

newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

instance Pretty Precise where
  prettyPrec d (Precise i) = prettyParen (d > 10) $ prettyString "Precise" <+> pretty i


runEnv :: PureEffects effects
       => Evaluator Precise value (Env Precise ': effects) a
       -> Evaluator Precise value (Reader (Environment Precise) ': effects) a
runEnv = reinterpret $ \case
  Lookup name -> askEnv >>= pure . fmap constraintValue . contextLookup name
  Bind name addr m -> local (|> (name ::: addr)) (runEnv (Evaluator m))
  Close fvs -> contextFilter ((`elem` fvs) . name) <$> askEnv

askEnv :: Member (Reader (Environment address)) effects => Evaluator address value effects (Environment address)
askEnv = ask


runAllocator :: ( Member Fresh effects
                , PureEffects effects
                )
             => Evaluator Precise value (Allocator Precise value ': effects) a
             -> Evaluator Precise value effects a
runAllocator = interpret $ \case
  Alloc _            -> Precise <$> fresh
  DerefCell cell     -> pure (getLast (foldMap (Last . Just) cell))
  AssignCell value _ -> pure (Set.singleton value)
