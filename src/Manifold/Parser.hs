{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Manifold.Parser where

import Control.Applicative (Alternative(..))
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Style

newtype Parser f a = Parser { runParser :: f a }
  deriving (Alternative, Applicative, CharParsing, Functor, Monad, Parsing)

instance TokenParsing f => TokenParsing (Parser f) where
  someSpace = Parser $ buildSomeSpaceParser someSpace haskellCommentStyle
  nesting = Parser . nesting . runParser
  highlight h = Parser . highlight h . runParser
