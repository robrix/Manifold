{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Manifold.Context
( Context
, contextLookup
, contextFind
, contextFilter
, emptyContext
, (|>)
) where

import Data.Bifunctor
import Data.Foldable (fold)
import Data.List
import Data.Module.Class
import Data.Semilattice.Lower
import Data.Semiring (Semiring(..))
import Manifold.Constraint
import Manifold.Name
import Manifold.Name.Annotated
import Manifold.Pretty

newtype Context var ty = Context { unContext :: [Constraint var ty] }
  deriving (Eq, Lower, Ord, Show)

contextLookup :: Named var => Name -> Context var ty -> Maybe (Constraint var ty)
contextLookup name = contextFind ((== name) . constraintName)

contextFind :: (Constraint var ty -> Bool) -> Context var ty -> Maybe (Constraint var ty)
contextFind predicate = find predicate . unContext


contextFilter :: (Constraint var ty -> Bool) -> Context var ty -> Context var ty
contextFilter keep = Context . filter keep . unContext


instance (Eq ty, Eq usage, Semigroup usage) => Semigroup (Context (Annotated usage) ty) where
  Context as <> Context bs = Context (zipWith go as bs)
    where go (Annotated name1 u1 ::: t1) (Annotated name2 u2 ::: t2)
            | name1 == name2, t1 == t2 = Annotated name1 (u1 <> u2) ::: t1
            | otherwise                = error "adding inequal contexts"


instance (Eq ty, Eq usage, Semiring usage) => Module usage (Context (Annotated usage) ty) where
  u ><< Context as = Context (map (first (fmap (u ><))) as)


instance (Pretty var, Pretty ty) => Pretty (Context var ty) where
  prettyPrec _ (Context []) = prettyString "◊"
  prettyPrec d (Context cs) = prettyParen (d > 0) $ fold (intersperse (comma <> space) (map (prettyPrec 0) (reverse cs)))


(|>) :: Context var ty -> Constraint var ty -> Context var ty
(|>) = fmap Context . flip (:) . unContext

infixl 5 |>

emptyContext :: Context var ty
emptyContext = Context []
