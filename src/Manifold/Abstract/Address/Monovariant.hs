{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Manifold.Abstract.Address.Monovariant where

import Manifold.Name
import Manifold.Pretty

newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)

instance Pretty Monovariant where
  prettyPrec d (Monovariant n) = prettyParen (d > 10) $ prettyString "Monovariant" <+> pretty n
