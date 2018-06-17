module Manifold.Name where

import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Manifold.Pretty

data Name
  = N String
  | Q String Name
  | I Int
  | O Operator
  deriving (Eq, Ord, Show)


data Operator
  = Prefix (NonEmpty String)
  | Postfix (NonEmpty String)
  | Infix (NonEmpty String)
  | Closed (NonEmpty String)
  deriving (Eq, Ord, Show)


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
  prettyPrec d (O op) = prettyPrec d op

instance Pretty Operator where
  prettyPrec d (Prefix ps) = prettyParen (d > 0) $ hsep (map ((<+> prettyString "_") . prettyString) (toList ps))
  prettyPrec d (Postfix ps) = prettyParen (d > 0) $ hsep (map ((prettyString "_" <+>) . prettyString) (toList ps))
  prettyPrec d (Infix ps) = prettyParen (d > 0) $ hsep (prettyString "_" : map ((<+> prettyString "_") . prettyString) (toList ps))
  prettyPrec d (Closed ps) = prettyParen (d > 0) $ hsep (intersperse (prettyString "_") (map prettyString (toList ps)))
