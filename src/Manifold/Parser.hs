{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Manifold.Parser
( Parser
-- * Parsing
, parseFile
, parseString
-- * Parsers
, whole
, term
) where

import Control.Applicative (Alternative(..))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.HashSet as HashSet
import Manifold.Constraint
import Manifold.Expr as Expr
import Manifold.Name (Name(..), Named(..))
import Manifold.Term as Term
import Manifold.Type as Type
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import qualified Text.Trifecta as Trifecta

newtype Parser f a = Parser { runParser :: f a }
  deriving (Alternative, Applicative, CharParsing, Functor, Monad, Parsing)

instance TokenParsing f => TokenParsing (Parser f) where
  someSpace = Parser $ buildSomeSpaceParser someSpace haskellCommentStyle
  nesting = Parser . nesting . runParser
  highlight h = Parser . highlight h . runParser

parseFile :: MonadIO m => Parser Trifecta.Parser a -> FilePath -> m (Maybe a)
parseFile (Parser p) = Trifecta.parseFromFile p

parseString :: Parser Trifecta.Parser a -> String -> Either String a
parseString (Parser p) = toResult . Trifecta.parseString p mempty

toResult :: Trifecta.Result a -> Either String a
toResult r = case r of
  Trifecta.Success a -> Right a
  Trifecta.Failure info -> Left (show (Trifecta._errDoc info))


whole :: TokenParsing m => m a -> m a
whole p = whiteSpace *> p <* eof


-- | Parse a term.
--
-- >>> parseString term "True"
-- Right (Term {unTerm = Intro (Bool True)})
-- >>> parseString term "False"
-- Right (Term {unTerm = Intro (Bool False)})
term :: (Monad m, TokenParsing m) => m Term
term = application
  where atom = choice [ true', false', try (rerep name <$> type'), var, let', lambda, tuple ]
        application = atom `chainl1` pure (#) <?> "function application"
        tuple = parens (chainl1 term (pair <$ comma) <|> pure unit) <?> "tuple"
        true'  = true  <$ preword "True"
        false' = false <$ preword "False"
        var = Term.var <$> name' <?> "variable"
        let' = makeLet <$  preword "let"
                       <*> constraint <* op "="
                       <*> term <* preword "in"
                       <*> term
                       <?> "let"
        lambda = foldr ((.) . abs') id <$  op "\\"
                                       <*> some constraint <* dot
                                       <*> term
                                       <?> "lambda"
        constraint = parens ((:::) <$> name' <* colon <*> type')


type' :: (Monad m, TokenParsing m) => m (Type Name)
type' = piType
  where piType = (.->) <$> constraint <* op "->" <*> piType
             <|> makePi <$> product <*> optional (op "->" *> piType)
        makePi ty1 Nothing = ty1
        makePi ty1 (Just ty2) = I (-1) ::: ty1 .-> ty2
        product = atom `chainl1` ((.*) <$ symbolic '*') <?> "product type"
        atom = choice [ boolT', unitT', typeT', tvar ]
        boolT' = boolT <$ preword "Bool"
        unitT' = unitT <$ preword "Unit"
        typeT' = typeT <$ preword "Type"
        tvar = Type.tvar <$> name' <?> "type variable"
        constraint = parens ((:::) <$> name' <* colon <*> type')


name' :: (Monad m, TokenParsing m) => m Name
name' = N <$> identifier

op :: TokenParsing m => String -> m String
op = token . highlight Operator . string

identifier :: (Monad m, TokenParsing m) => m String
identifier =  ident (IdentifierStyle "identifier" letter alphaNum reservedWords Identifier ReservedIdentifier)
          <|> try ((:[]) <$> token (parens (highlight Operator (oneOf ".,"))))

reservedWords :: HashSet.HashSet String
reservedWords =  HashSet.fromList [ "exl", "exr", "let", "in" ]

preword :: TokenParsing m => String -> m String
preword s = token (highlight ReservedIdentifier (string s <* notFollowedBy alphaNum)) <?> s
