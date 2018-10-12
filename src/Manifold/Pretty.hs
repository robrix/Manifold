{-# LANGUAGE DataKinds, DefaultSignatures, FlexibleContexts, FlexibleInstances, TypeOperators #-}
module Manifold.Pretty
( Pretty(..)
, Pretty1(..)
, pretty
, prettyShow
, prettyPrint
, prettyString
, prettyParen
, module Doc
, putDoc
) where

import Control.Effect (SomeError(..))
import Data.Text.Prettyprint.Doc as Doc hiding (Pretty(..))
import qualified Data.Text.Prettyprint.Doc as Doc
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Doc
import GHC.Generics ((:+:)(..))
import System.Console.Terminal.Size as Size
import System.IO (stdout)

class Pretty a where
  prettyPrec :: Int -> a -> Doc ann
  default prettyPrec :: Show a => Int -> a -> Doc ann
  prettyPrec d a = Doc.pretty (showsPrec d a "")

class Pretty1 f where
  liftPrettyPrec :: (Int -> a -> Doc ann) -> Int -> f a -> Doc ann

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

instance Pretty1 err => Pretty (SomeError err) where
  prettyPrec d (SomeError err) = liftPrettyPrec (const (const mempty)) d err

instance Pretty1 [] where
  liftPrettyPrec pp _ = list . map (pp 0)

instance (Pretty1 f, Pretty1 g) => Pretty1 (f :+: g) where
  liftPrettyPrec pp d (L1 l) = liftPrettyPrec pp d l
  liftPrettyPrec pp d (R1 r) = liftPrettyPrec pp d r
