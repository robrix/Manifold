{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Manifold.Parser where

import Control.Applicative (Alternative(..))
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Style
import qualified Text.Trifecta as Trifecta

newtype Parser f a = Parser { runParser :: f a }
  deriving (Alternative, Applicative, CharParsing, Functor, Monad, Parsing)

instance TokenParsing f => TokenParsing (Parser f) where
  someSpace = Parser $ buildSomeSpaceParser someSpace haskellCommentStyle
  nesting = Parser . nesting . runParser
  highlight h = Parser . highlight h . runParser

parseString :: Parser Trifecta.Parser a -> String -> Either [String] a
parseString (Parser p) = toResult . Trifecta.parseString p mempty

toResult :: Trifecta.Result a -> Either [String] a
toResult r = case r of
  Trifecta.Success a -> Right a
  Trifecta.Failure info -> Left [show (Trifecta._errDoc info)]
