{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Manifold.Context where

import Data.Module.Class
import Data.Semiring (Semiring(..))
import Manifold.Name
import Manifold.Presyntax

data Context usage
  = Empty
  | Context usage :> Constraint usage
  deriving (Eq, Ord, Show)

infixl 5 :>


contextFind :: Name -> Context usage -> Maybe (Constraint usage)
contextFind name (context :> constraint)
  | fst (constraintBinding constraint) == name = Just constraint
  | otherwise                                  = contextFind name context
contextFind _    Empty                         = Nothing


instance (Eq usage, Semigroup usage) => Semigroup (Context usage) where
  Empty <> Empty = Empty
  (ctx1 :> ((name1, u1) ::: t1)) <> (ctx2 :> ((name2, u2) ::: t2))
    | name1 == name2
    , t1 == t2
    = (ctx1 <> ctx2) :> ((name1, u1 <> u2) ::: t1)
  _ <> _ = error "adding inequal contexts"


instance (Eq usage, Semiring usage) => Module usage (Context usage) where
  _  ><< Empty = Empty
  u1 ><< (ctx :> ((name, u2) ::: t)) = (u1 ><< ctx) :> ((name, u1 >< u2) ::: t)
