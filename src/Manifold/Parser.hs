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
import qualified Manifold.Term as Term
import qualified Manifold.Type as Type
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
term :: (Monad m, TokenParsing m) => m Term.Term
term = application
  where atom = choice [ true', false', try (rerep name <$> type'), var, let', lambda, tuple ]
        application = atom `chainl1` pure (Term.#) <?> "function application"
        tuple = parens (chainl1 term (Term.pair <$ comma) <|> pure Term.unit) <?> "tuple"
        var = Term.var <$> name' <?> "variable"
        lambda = foldr ((.) . Term.abs') id <$  op "\\"
                                            <*> some constraint <* dot
                                            <*> term
                                            <?> "lambda"

true', false' :: (Monad m, TokenParsing m) => m Term.Term

-- $
-- >>> parseString true' "True"
-- Right (Term {unTerm = Intro (Bool True)})
true' = Term.true <$ preword "True"

-- $
-- >>> parseString false' "False"
-- Right (Term {unTerm = Intro (Bool False)})
false' = Term.false <$ preword "False"

let' :: (Monad m, TokenParsing m) => m Term.Term
let' = Term.makeLet <$  preword "let"
                    <*> constraint <* op "="
                    <*> term <* preword "in"
                    <*> term
                    <?> "let"

constraint :: (Monad m, TokenParsing m) => m (Constraint Name (Type.Type Name))
constraint = parens ((:::) <$> name' <* colon <*> type')

type' :: (Monad m, TokenParsing m) => m (Type.Type Name)
type' = piType
  where piType = (Type..->) <$> constraint <* op "->" <*> piType
                 <|> makePi <$> product <*> optional (op "->" *> piType)
        makePi ty1 Nothing = ty1
        makePi ty1 (Just ty2) = I (-1) ::: ty1 Type..-> ty2
        product = atom `chainl1` ((Type..*) <$ symbolic '*') <?> "product type"
        atom = choice [ boolT', unitT', typeT', tvar ]
        tvar = Type.tvar <$> name' <?> "type variable"
        constraint = parens ((:::) <$> name' <* colon <*> type')

boolT', unitT', typeT' :: (Monad m, TokenParsing m) => m (Type.Type Name)
boolT' = Type.boolT <$ preword "Bool"
unitT' = Type.unitT <$ preword "Unit"
typeT' = Type.typeT <$ preword "Type"

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
