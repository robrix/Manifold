{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Manifold.Parser where

import Control.Applicative (Alternative(..))
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

parseString :: Parser Trifecta.Parser a -> String -> Either String a
parseString (Parser p) = toResult . Trifecta.parseString p mempty

toResult :: Trifecta.Result a -> Either String a
toResult r = case r of
  Trifecta.Success a -> Right a
  Trifecta.Failure info -> Left (show (Trifecta._errDoc info))


whole :: TokenParsing m => m a -> m a
whole p = whiteSpace *> p <* eof


term :: (Monad m, TokenParsing m) => m Term
term = application
  where atom = choice [ tuple, true', false', rerep name <$> type', var, let', lambda ]
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
type' = product
  where product = chainl1 atom ((.*) <$ symbolic '*') <?> "product type"
        atom = choice [ boolT', unitT', typeT', tvar ]
        boolT' = boolT <$ preword "Bool"
        unitT' = unitT <$ preword "Unit"
        typeT' = typeT <$ preword "Type"
        tvar = Type.tvar <$> name' <?> "type variable"


-- piType :: (Monad m, TokenParsing m) => m Type
-- piType = fmap toPi $ ((:[]) <$> argument) `chainr1` ((++) <$ op "->")
--   where exponential arg = case arg of
--           Named name ty -> makePi name ty
--           Unnamed ty -> (.->.) ty
--         codomain res = case res of
--           Named name ty -> Expr.var name `as` ty
--           Unnamed ty -> ty
--         toPi components = foldr exponential (codomain (Prelude.last components)) (Prelude.init components)
--
-- argument :: (Monad m, TokenParsing m) => m Argument
-- argument =  try (parens (Named <$> name' <* colon <*> type'))
--         <|>            Unnamed <$> sumType
--         <?> "argument"
--
-- data Argument = Named Name Type | Unnamed Type


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
