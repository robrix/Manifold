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
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.HashSet as HashSet
import Manifold.Constraint
import Manifold.Declaration
import Manifold.Module (Module(Module))
import Manifold.Name (Name(..))
import qualified Manifold.Term as Term
import qualified Manifold.Type as Type
import Prelude hiding (product)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Token
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import Text.Trifecta hiding (Parser, parseString)
import qualified Text.Trifecta as Trifecta
import Text.Trifecta.Delta
import Text.Trifecta.Indentation

newtype Parser a = Parser { runParser :: IndentationParserT Token Trifecta.Parser a }
  deriving (Alternative, Applicative, CharParsing, DeltaParsing, Functor, IndentationParsing, LookAheadParsing, MarkParsing Delta, Monad, MonadPlus, Parsing)

instance TokenParsing Parser where
  someSpace = Parser $ buildSomeSpaceParser someSpace haskellCommentStyle
  nesting = Parser . nesting . runParser
  highlight h = Parser . highlight h . runParser

parseFile :: MonadIO m => Parser a -> FilePath -> m (Maybe a)
parseFile (Parser p) = Trifecta.parseFromFile (evalIndentationParserT p indentation)

parseString :: Parser a -> String -> Either String a
parseString (Parser p) = toResult . Trifecta.parseString (evalIndentationParserT p indentation) mempty

indentation :: IndentationState
indentation = mkIndentationState 0 infIndentation False Eq

toResult :: Trifecta.Result a -> Either String a
toResult r = case r of
  Trifecta.Success a -> Right a
  Trifecta.Failure info -> Left (show (Trifecta._errDoc info))


whole :: TokenParsing m => m a -> m a
whole p = whiteSpace *> p <* eof


module' :: (Monad m, TokenParsing m) => m (Module Name (Term.Term Name))
module' = Module <$  keyword "module" <*> moduleName <* keyword "where"
                 <*> many import'
                 <*> many declaration


import' :: (Monad m, TokenParsing m) => m Name
import' = keyword "import" *> moduleName

declaration, binding, dataType :: (Monad m, TokenParsing m) => m (Declaration Name (Term.Term Name))

declaration = choice [ binding, dataType ]

binding = runUnlined (do
  name <- identifier
  ty <- colon *> type' <* some newline
  body <- token (highlight Identifier (string name)) *> op "=" *> term <* some newline
  pure (Binding (N name ::: ty) body))

dataType = runUnlined $ do
  name <- keyword "data" *> constructorName
  ty <- colon *> type'
  constructors <- [] <$ some newline <|> keyword "where" *> token newline *> many (constructor <* token newline)
  pure (Datatype (name ::: ty) constructors)
  where constructor = (:::) <$> constructorName <* colon <*> type' <* token newline



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
                                       <*> some pattern <* dot
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


pattern, name, constructorName, moduleName :: (Monad m, TokenParsing m) => m Name

pattern = name <|> I (-1) <$ token (string "_")

name = N <$> identifier
constructorName = N <$> typeIdentifier

moduleName = token (runUnspaced name')
  where name' = makeN <$> typeIdentifier <*> optional (dot *> name')
        makeN s Nothing  = N s
        makeN s (Just n) = Q s n

identifier, typeIdentifier :: (Monad m, TokenParsing m) => m String

identifier     = ident (IdentifierStyle "identifier" letter alphaNum reservedWords Identifier ReservedIdentifier)
typeIdentifier = ident (IdentifierStyle "type identifier" upper alphaNum reservedWords Identifier ReservedIdentifier)

reservedWords :: HashSet.HashSet String
reservedWords =  HashSet.fromList [ "exl", "exr", "let", "in", "module", "where", "import", "data" ]

keyword, op :: TokenParsing m => String -> m String

keyword s = token (highlight ReservedIdentifier (string s <* notFollowedBy alphaNum)) <?> s

op s = token (highlight Operator (string s)) <?> s
