{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Manifold.Context where

import Data.Module.Class
import Data.Semiring (Semiring(..))
import Manifold.Binding
import Manifold.Constraint
import Manifold.Name

data Context usage recur
  = Empty
  | Context usage recur :> Constraint usage recur
  deriving (Eq, Ord, Show)

infixl 5 :>


contextLookup :: Name -> Context usage recur -> Maybe (Constraint usage recur)
contextLookup name = go
  where go (context :> constraint)
          | constraintName constraint == name = Just constraint
          | otherwise                         = contextLookup name context
        go Empty                              = Nothing


contextFilter :: (Constraint usage recur -> Bool) -> Context usage recur -> Context usage recur
contextFilter keep = go
  where go (context :> constraint)
          | keep constraint = go context :> constraint
          | otherwise       = go context
        go Empty            = Empty


instance (Eq recur, Eq usage, Semigroup usage) => Semigroup (Context usage recur) where
  Empty <> Empty = Empty
  (ctx1 :> (Binding name1 u1 ::: t1)) <> (ctx2 :> (Binding name2 u2 ::: t2))
    | name1 == name2
    , t1 == t2
    = (ctx1 <> ctx2) :> (Binding name1 (u1 <> u2) ::: t1)
  _ <> _ = error "adding inequal contexts"


instance (Eq recur, Eq usage, Semiring usage) => Module usage (Context usage recur) where
  _  ><< Empty = Empty
  u1 ><< (ctx :> (Binding name u2 ::: t)) = (u1 ><< ctx) :> (Binding name (u1 >< u2) ::: t)


(|>) :: Context usage recur -> Constraint usage recur -> Context usage recur
(|>) = (:>)

infixl 5 |>
