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
) where

import Data.Text.Prettyprint.Doc as Doc hiding (Pretty(..))
import qualified Data.Text.Prettyprint.Doc as Doc
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Doc
import System.Console.Terminal.Size as Size
import System.IO (stdout)

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

putDoc :: Doc Doc.AnsiStyle -> IO ()
putDoc doc = do
  options <- layoutOptions
  Doc.renderIO stdout (layoutPretty options (doc <> line))

layoutOptions :: IO LayoutOptions
layoutOptions = do
  s <- maybe 80 Size.width <$> size
  pure LayoutOptions { layoutPageWidth = AvailablePerLine s 1 }


prettyString :: String -> Doc ann
prettyString = Doc.pretty

prettyParen :: Bool -> Doc ann -> Doc ann
prettyParen True = parens
prettyParen False = id


instance Pretty () where prettyPrec _ _ = mempty

instance Pretty a => Pretty [a] where
  prettyPrec _ = list . map (prettyPrec 0)

instance Pretty Int
