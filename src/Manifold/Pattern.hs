{-# LANGUAGE LambdaCase #-}
module Manifold.Pattern where

import Manifold.Name
import Manifold.Pretty

data Pattern var
  = Wildcard
  | Variable var
  | Constructor var [Pattern var]
  deriving (Eq, Ord, Show)

pair :: Pattern Name -> Pattern Name -> Pattern Name
pair a b = Constructor (N "Pair") [a, b]

unit :: Pattern Name
unit = Constructor (N "Unit") []

instance Pretty var => Pretty (Pattern var) where
  prettyPrec d = \case
    Wildcard -> prettyString "_"
    Variable name -> prettyPrec d name
    Constructor name patterns -> prettyParen (d > 10) $ prettyPrec 10 name <> foldMap ((space <>) . prettyPrec 11) patterns
