{-# LANGUAGE DeriveFoldable, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Manifold.Context
( Context
, (|>)
, Linear
, (<|)
, contextLookup
, contextFind
, contextFilter
) where

import Data.Bifunctor
import Data.Module.Class
import Data.Semilattice.Lower
import Data.Semiring (Semiring(..))
import Manifold.Constraint
import Manifold.Name
import Manifold.Name.Annotated
import Manifold.Pretty

data Context prop
  = CEmpty
  | Context prop :|> prop
  deriving (Eq, Foldable, Ord, Show)

infixl 4 :|>

(|>) :: Context prop -> prop -> Context prop
(|>) = (:|>)

infixl 4 |>

data Linear prop
  = LEmpty
  | prop :<| Linear prop

infixr 5 :<|

(<|) :: prop -> Linear prop -> Linear prop
(<|) = (:<|)

infixr 5 <|


contextLookup :: Named prop => Name -> Context prop -> Maybe prop
contextLookup needle = contextFind ((== needle) . name)

contextFind :: (prop -> Bool) -> Context prop -> Maybe prop
contextFind _         CEmpty    = Nothing
contextFind predicate (g :|> c)
  | predicate c = Just c
  | otherwise   = contextFind predicate g


contextFilter :: (prop -> Bool) -> Context prop -> Context prop
contextFilter _    CEmpty    = CEmpty
contextFilter keep (g :|> c)
  | keep c    = g' :|> c
  | otherwise = g'
  where g' = contextFilter keep g


instance Lower (Context prop) where
  lowerBound = CEmpty

instance (Eq ty, Eq usage, Semigroup usage) => Semigroup (Context (Constraint (Annotated usage) ty)) where
  a1 <> a2
    | CEmpty <- a1
    , CEmpty <- a2
    = CEmpty
    | g1 :|> Annotated n1 u1 ::: t1 <- a1
    , g2 :|> Annotated n2 u2 ::: t2 <- a2
    , n1 == n2, t1 == t2
    = g1 <> g2 :|> Annotated n1 (u1 <> u2) ::: t1
    | otherwise
    = error "adding inequal contexts"

instance (Eq ty, Eq usage, Semigroup usage) => Monoid (Context (Constraint (Annotated usage) ty)) where
  mappend = (<>)
  mempty = lowerBound


instance (Eq ty, Eq usage, Semiring usage) => Module usage (Context (Constraint (Annotated usage) ty)) where
  _ ><< CEmpty    = CEmpty
  u ><< (g :|> c) = (u ><< g) :|> first (fmap (u ><)) c


instance Pretty prop => Pretty (Context prop) where
  prettyPrec _ CEmpty = prettyString "◊"
  prettyPrec d (g :|> c) = prettyParen (d > 4) $ prettyPrec 4 g <> comma <+> prettyPrec 5 c


instance Lower (Linear prop) where
  lowerBound = LEmpty
