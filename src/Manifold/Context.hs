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

newtype Context var ty = Context { unContext :: [Constraint var ty] }
  deriving (Eq, Ord, Show)

contextLookup :: Named var => Name -> Context var ty -> Maybe (Constraint var ty)
contextLookup name = find ((== name) . constraintName) . unContext


contextFilter :: (Constraint var ty -> Bool) -> Context var ty -> Context var ty
contextFilter keep = Context . filter keep . unContext


instance (Eq ty, Eq usage, Semigroup usage) => Semigroup (Context (Binding usage) ty) where
  Context as <> Context bs = Context (zipWith go as bs)
    where go (Binding name1 u1 ::: t1) (Binding name2 u2 ::: t2)
            | name1 == name2, t1 == t2 = Binding name1 (u1 <> u2) ::: t1
            | otherwise                = error "adding inequal contexts"


instance (Eq ty, Eq usage, Semiring usage) => Module usage (Context (Binding usage) ty) where
  u ><< Context as = Context (map (first (fmap (u ><))) as)


(|>) :: Context var ty -> Constraint var ty -> Context var ty
(|>) = fmap Context . flip (:) . unContext

infixl 5 |>

emptyContext :: Context var ty
emptyContext = Context []
