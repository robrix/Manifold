{-# LANGUAGE DefaultSignatures #-}
module Manifold.Pretty
( Pretty(..)
, pretty
, prettyShow
, prettyPrint
, prettyString
, prettyParen
, module Doc
, putDoc
, putDocW
) where

import Data.Text.Prettyprint.Doc as Doc hiding (Pretty(..))
import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc.Util

class Pretty a where
  prettyPrec :: Int -> a -> Doc ann
  default prettyPrec :: Show a => Int -> a -> Doc ann
  prettyPrec d a = Doc.pretty (showsPrec d a "")

pretty :: Pretty a => a -> Doc ann
pretty = prettyPrec 0

prettyShow :: Pretty a => a -> String
prettyShow = show . pretty

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putDoc . pretty


prettyString :: String -> Doc ann
prettyString = Doc.pretty

prettyParen :: Bool -> Doc ann -> Doc ann
prettyParen True = parens
prettyParen False = id


instance Pretty () where prettyPrec _ _ = mempty

instance Pretty a => Pretty [a] where
  prettyPrec _ = list . map (prettyPrec 0)
