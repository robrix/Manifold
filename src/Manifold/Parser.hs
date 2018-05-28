{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Manifold.Parser
( Parser
-- * Parsing
, parseFile
, parseString
-- * Parsers
, whole
, module'
, term
) where

import Control.Applicative (Alternative(..))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.HashSet as HashSet
import Manifold.Constraint
import Manifold.Declaration
import Manifold.Module
import Manifold.Name (Name(..))
import qualified Manifold.Term as Term
import qualified Manifold.Type as Type
import Prelude hiding (product)
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


module' :: (Monad m, TokenParsing m) => m (Module Name (Term.Term Name))
module' = Module <$ keyword "module" <*> qname <* keyword "where" <*> many declaration


declaration :: (Monad m, TokenParsing m) => m (Declaration Name (Term.Term Name))
declaration = runUnlined (do
  name <- identifier
  ty' <- colon *> type' <* some newline
  body <- token (highlight Identifier (string name)) *> op "=" *> term <* some newline
  pure (Declaration (N name ::: ty') body))


term, application, true, false, var, let', lambda, tuple :: (Monad m, TokenParsing m) => m (Term.Term Name)

-- | Parse a term.
term = application

application = atom `chainl1` pure (Term.#) <?> "function application"
  where atom = choice [ true, false, var, let', lambda, tuple ]

-- $
-- >>> parseString true "True"
-- Right (Term {unTerm = Intro (Bool True)})
true = Term.true <$ keyword "True"

-- $
-- >>> parseString false "False"
-- Right (Term {unTerm = Intro (Bool False)})
false = Term.false <$ keyword "False"

var = Term.var <$> name <?> "variable"

let' = Term.makeLet <$  keyword "let"
                    <*> name <* op "="
                    <*> term <* keyword "in"
                    <*> term
                    <?> "let"

lambda = foldr ((.) . Term.makeAbs) id <$  op "\\"
                                       <*> some name <* dot
                                       <*> term
                                       <?> "lambda"

-- $
-- >>> parseString tuple "()"
-- Right (Term {unTerm = Intro Unit})
-- >>> parseString tuple "(True)"
-- Right (Term {unTerm = Intro (Bool True)})
-- >>> parseString tuple "(True, False)"
-- Right (Term {unTerm = Intro (Pair (Term {unTerm = Intro (Bool True)}) (Term {unTerm = Intro (Bool False)}))})
-- >>> parseString tuple "((), True, False)"
-- Right (Term {unTerm = Intro (Pair (Term {unTerm = Intro (Pair (Term {unTerm = Intro Unit}) (Term {unTerm = Intro (Bool True)}))}) (Term {unTerm = Intro (Bool False)}))})
tuple = parens (chainl1 term (Term.pair <$ comma) <|> pure Term.unit) <?> "tuple"

constraint :: (Monad m, TokenParsing m) => m (Constraint Name (Type.Type Name))
constraint = (:::) <$> name <* colon <*> type'

type', piType, product, boolT, unitT, typeT, tvar :: (Monad m, TokenParsing m) => m (Type.Type Name)

type' = piType

piType = (Type..->) <$> parens constraint <* op "->" <*> piType
  <|> makePi <$> product <*> optional (op "->" *> piType)
  where makePi ty1 Nothing = ty1
        makePi ty1 (Just ty2) = I (-1) ::: ty1 Type..-> ty2

product = atom `chainl1` ((Type..*) <$ symbolic '*') <?> "product type"
  where atom = choice [ boolT, unitT, typeT, tvar ]

-- $
-- >>> parseString boolT "Bool"
-- Right (Type {unType = Intro BoolT})
boolT = Type.boolT <$ keyword "Bool"

-- $
-- >>> parseString unitT "Unit"
-- Right (Type {unType = Intro UnitT})
unitT = Type.unitT <$ keyword "Unit"

-- $
-- >>> parseString typeT "Type"
-- Right (Type {unType = Intro TypeT})
typeT = Type.typeT <$ keyword "Type"

tvar = Type.tvar <$> name <?> "type variable"


name :: (Monad m, TokenParsing m) => m Name
name = N <$> identifier

qname :: (Monad m, TokenParsing m) => m Name
qname = token (runUnspaced name')
  where name' = makeN <$> typeIdentifier <*> optional (dot *> name')
            <|>     N <$> identifier
        makeN s Nothing  = N s
        makeN s (Just n) = Q s n

op :: TokenParsing m => String -> m String
op = token . highlight Operator . string

typeIdentifier :: (Monad m, TokenParsing m) => m String
typeIdentifier =  ident (IdentifierStyle "type identifier" upper alphaNum reservedWords Identifier ReservedIdentifier)

identifier :: (Monad m, TokenParsing m) => m String
identifier =  ident (IdentifierStyle "identifier" letter alphaNum reservedWords Identifier ReservedIdentifier)

reservedWords :: HashSet.HashSet String
reservedWords =  HashSet.fromList [ "exl", "exr", "let", "in", "module", "where" ]

keyword :: TokenParsing m => String -> m String
keyword s = token (highlight ReservedIdentifier (string s <* notFollowedBy alphaNum)) <?> s
