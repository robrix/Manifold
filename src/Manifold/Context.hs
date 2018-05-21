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

newtype Context var recur = Context { unContext :: [Constraint var recur] }
  deriving (Eq, Ord, Show)

contextLookup :: Named var => Name -> Context var recur -> Maybe (Constraint var recur)
contextLookup name = find ((== name) . constraintName) . unContext


contextFilter :: (Constraint var recur -> Bool) -> Context var recur -> Context var recur
contextFilter keep = Context . filter keep . unContext


instance (Eq recur, Eq usage, Semigroup usage) => Semigroup (Context (Binding usage) recur) where
  Context as <> Context bs = Context (zipWith go as bs)
    where go (Binding name1 u1 ::: t1) (Binding name2 u2 ::: t2)
            | name1 == name2, t1 == t2 = Binding name1 (u1 <> u2) ::: t1
            | otherwise                = error "adding inequal contexts"


instance (Eq recur, Eq usage, Semiring usage) => Module usage (Context (Binding usage) recur) where
  u ><< Context as = Context (map (first (fmap (u ><))) as)


(|>) :: Context var recur -> Constraint var recur -> Context var recur
(|>) = fmap Context . flip (:) . unContext

infixl 5 |>

emptyContext :: Context var recur
emptyContext = Context []
