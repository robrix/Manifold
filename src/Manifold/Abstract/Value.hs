{-# LANGUAGE DeriveFunctor, FlexibleContexts, KindSignatures, MultiParamTypeClasses #-}
module Manifold.Abstract.Value where

import Manifold.Abstract.Evaluator
import Manifold.Name

lambda :: (Member (Function value) sig, Carrier sig carrier) => Name -> Evaluator address value carrier value -> Evaluator address value carrier value
lambda name body = send (Lambda name body gen)

apply :: (Member (Function value) sig, Carrier sig carrier) => value -> value -> Evaluator address value carrier value
apply f a = send (Apply f a gen)

data Function value m k
  = Lambda Name (m value) (value -> k)
  | Apply value value (value -> k)
  deriving (Functor)

instance HFunctor (Function value) where
  hfmap f (Lambda n m k) = Lambda n (f m) k
  hfmap _ (Apply f a k)  = Apply f a      k


construct :: (Member (Data value) sig, Carrier sig carrier) => Name -> [value] -> Evaluator address value carrier value
construct name values = send (Construct name values gen)

deconstruct :: (Member (Data value) sig, Carrier sig carrier) => value -> Evaluator address value carrier (Name, [value])
deconstruct value = send (Deconstruct value gen)

data Data value (m :: * -> *) k
  = Construct Name [value] (value -> k)
  | Deconstruct value ((Name, [value]) -> k)
  deriving (Functor)

instance HFunctor (Data value) where
  hfmap _ (Construct name values k) = Construct name values k
  hfmap _ (Deconstruct value     k) = Deconstruct value k

instance Effect (Data value) where
  handle state handler (Construct name values k) = Construct name values (handler . (<$ state) . k)
  handle state handler (Deconstruct value k)     = Deconstruct value (handler . (<$ state) . k)
