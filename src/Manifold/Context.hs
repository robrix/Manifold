{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Manifold.Context
( Context
, contextLookup
, contextFilter
, emptyContext
, (|>)
) where

import Data.Bifunctor
import Data.List
import Data.Module.Class
import Data.Semiring (Semiring(..))
import Manifold.Binding
import Manifold.Constraint
import Manifold.Name

newtype Context usage recur = Context { unContext :: [Constraint usage recur] }
  deriving (Eq, Ord, Show)

contextLookup :: Name -> Context usage recur -> Maybe (Constraint usage recur)
contextLookup name = find ((== name) . constraintName) . unContext


contextFilter :: (Constraint usage recur -> Bool) -> Context usage recur -> Context usage recur
contextFilter keep = Context . filter keep . unContext


instance (Eq recur, Eq usage, Semigroup usage) => Semigroup (Context usage recur) where
  Context as <> Context bs = Context (zipWith go as bs)
    where go (Binding name1 u1 ::: t1) (Binding name2 u2 ::: t2)
            | name1 == name2, t1 == t2 = Binding name1 (u1 <> u2) ::: t1
            | otherwise                = error "adding inequal contexts"


instance (Eq recur, Eq usage, Semiring usage) => Module usage (Context usage recur) where
  u ><< Context as = Context (map (first (u ><)) as)


(|>) :: Context usage recur -> Constraint usage recur -> Context usage recur
(|>) = fmap Context . flip (:) . unContext

infixl 5 |>

emptyContext :: Context usage recur
emptyContext = Context []
