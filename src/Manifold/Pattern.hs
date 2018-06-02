{-# LANGUAGE LambdaCase #-}
module Manifold.Pattern where

import Manifold.Name
import Manifold.Pretty

data Pattern
  = Wildcard
  | Variable Name
  | Constructor Name [Pattern]
  deriving (Eq, Ord, Show)

pair :: Pattern -> Pattern -> Pattern
pair a b = Constructor (N "Pair") [a, b]

unit :: Pattern
unit = Constructor (N "Unit") []

instance Pretty Pattern where
  prettyPrec d = \case
    Wildcard -> prettyString "_"
    Variable name -> prettyPrec d name
    Constructor name patterns -> prettyParen (d > 10) $ prettyPrec 10 name <> foldMap ((space <>) . prettyPrec 11) patterns
