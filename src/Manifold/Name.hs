{-# LANGUAGE FlexibleContexts #-}
module Manifold.Name where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Manifold.Pretty

data Name
  = N String
  | Q String Name
  | I Int
  deriving (Eq, Ord, Show)

gensym :: (Effectful m, Member Fresh effects) => m effects Name
gensym = raiseEff (I <$> fresh)


class Named n where
  name :: n -> Name
  setName :: Name -> n -> n

instance Named Name where
  name = id
  setName = const


instance Pretty Name where
  prettyPrec _ (N s) = prettyString s
  prettyPrec _ (Q s n) = prettyString s <> dot <> pretty n
  prettyPrec _ (I n) = (if n < 0 then prettyString "_" else mempty) <> prettyString (replicate (succ i) (alphabet !! r))
    where alphabet = ['a'..'z']
          (i, r) = (if n < 0 then abs (succ n) else n) `divMod` length alphabet
