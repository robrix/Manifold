{-# LANGUAGE LambdaCase #-}
module Manifold.Pattern where

import Manifold.Name
import Manifold.Pretty

data Pattern
  = Wildcard
  | Variable Name
  | Constructor Name [Pattern]
  deriving (Eq, Ord, Show)

instance Pretty Pattern where
  prettyPrec d = \case
    Wildcard -> showChar '_'
    Variable name -> prettyPrec d name
    Constructor name patterns -> showParen (d > 10) $ prettyPrec 10 name . foldr (.) id (map (fmap (showChar ' ') . prettyPrec 11) patterns)
