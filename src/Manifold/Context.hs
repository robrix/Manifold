{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Manifold.Context
( Context
, contextLookup
, contextFind
, contextFilter
, (|>)
) where

import Data.Bifunctor
import Data.Module.Class
import Data.Semilattice.Lower
import Data.Semiring (Semiring(..))
import Manifold.Constraint
import Manifold.Name
import Manifold.Name.Annotated
import Manifold.Pretty

data Context var ty
  = Empty
  | Context var ty :|> Constraint var ty
  deriving (Eq, Ord, Show)

infixl 4 :|>

(|>) :: Context var ty -> Constraint var ty -> Context var ty
(|>) = (:|>)

infixl 4 |>


contextLookup :: Named var => Name -> Context var ty -> Maybe (Constraint var ty)
contextLookup name = contextFind ((== name) . constraintName)

contextFind :: (Constraint var ty -> Bool) -> Context var ty -> Maybe (Constraint var ty)
contextFind _         Empty     = Nothing
contextFind predicate (g :|> c)
  | predicate c = Just c
  | otherwise   = contextFind predicate g


contextFilter :: (Constraint var ty -> Bool) -> Context var ty -> Context var ty
contextFilter _    Empty     = Empty
contextFilter keep (g :|> c)
  | keep c    = g' :|> c
  | otherwise = g'
  where g' = contextFilter keep g


instance Lower (Context var ty) where
  lowerBound = Empty

instance (Eq ty, Eq usage, Semigroup usage) => Semigroup (Context (Annotated usage) ty) where
  a1 <> a2
    | Empty <- a1
    , Empty <- a2
    = Empty
    | g1 :|> Annotated n1 u1 ::: t1 <- a1
    , g2 :|> Annotated n2 u2 ::: t2 <- a2
    , n1 == n2, t1 == t2
    = g1 <> g2 :|> Annotated n1 (u1 <> u2) ::: t1
    | otherwise
    = error "adding inequal contexts"

instance (Eq ty, Eq usage, Semigroup usage) => Monoid (Context (Annotated usage) ty) where
  mappend = (<>)
  mempty = lowerBound


instance (Eq ty, Eq usage, Semiring usage) => Module usage (Context (Annotated usage) ty) where
  _ ><< Empty     = Empty
  u ><< (g :|> c) = (u ><< g) :|> first (fmap (u ><)) c


instance (Pretty var, Pretty ty) => Pretty (Context var ty) where
  prettyPrec _ Empty = prettyString "◊"
  prettyPrec d (g :|> c) = prettyParen (d > 4) $ prettyPrec 4 g <> comma <+> prettyPrec 5 c
