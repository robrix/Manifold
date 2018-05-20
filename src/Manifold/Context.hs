module Manifold.Context where

import Manifold.Presyntax

data Context usage
  = Empty
  | Context usage :> Constraint usage
  deriving (Eq, Ord, Show)

infixl 5 :>


instance (Eq usage, Semigroup usage) => Semigroup (Context usage) where
  Empty <> Empty = Empty
  (ctx1 :> name1 :@ u1 ::: t1) <> (ctx2 :> name2 :@ u2 ::: t2)
    | name1 == name2
    , t1 == t2
    = (ctx1 <> ctx2) :> name1 :@ (u1 <> u2) ::: t1
  _ <> _ = error "adding inequal contexts"
