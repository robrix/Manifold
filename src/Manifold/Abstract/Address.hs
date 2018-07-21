{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Manifold.Abstract.Address where

import Manifold.Name
import Manifold.Pretty

newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

instance Pretty Precise where
  prettyPrec d (Precise i) = prettyParen (d > 10) $ prettyString "Precise" <+> pretty i


newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)

instance Pretty Monovariant where
  prettyPrec d (Monovariant n) = prettyParen (d > 10) $ prettyString "Monovariant" <+> pretty n
