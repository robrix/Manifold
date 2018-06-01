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
-- import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.HashSet as HashSet
import Manifold.Constraint
import Manifold.Declaration
import Manifold.Module (Module(Module))
import Manifold.Name (Name(..))
import Manifold.Pattern as Pattern
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

type Parser = IndentationParserT Char Inner

newtype Inner a = Inner { runInner :: Trifecta.Parser a }
  deriving (Alternative, Applicative, CharParsing, DeltaParsing, Functor, LookAheadParsing, MarkParsing Delta, Monad, MonadPlus, Parsing)

instance TokenParsing Inner where
  someSpace = Inner $ buildSomeSpaceParser (skipSome (satisfy isSpace)) haskellCommentStyle
  nesting = Inner . nesting . runInner
  highlight h = Inner . highlight h . runInner

parseFile :: MonadIO m => Parser a -> FilePath -> m (Maybe a)
parseFile p = parseFromFile (runInner (whiteSpace *> evalCharIndentationParserT p indentst))

parseString :: Parser a -> String -> Either String a
parseString p = toResult . Trifecta.parseString (runInner (evalCharIndentationParserT p indentst)) mempty -- directed

toResult :: Trifecta.Result a -> Either String a
toResult r = case r of
  Trifecta.Success a -> Right a
  Trifecta.Failure info -> Left (show (Trifecta._errDoc info))


evalCharIndentationParserT :: Monad m => IndentationParserT Char m a -> IndentationState -> m a
evalCharIndentationParserT = evalIndentationParserT

-- evalTokenIndentationParserT :: Monad m => IndentationParserT Token m a -> IndentationState -> m a
-- evalTokenIndentationParserT = evalIndentationParserT

-- directed = Directed BS.empty 0 0 0 0
indentst = mkIndentationState 0 infIndentation True Gt


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

binding = runUnlined $ do
  name <- identifier
  ty <- colon *> type' <* some nl
  body <- token (highlight Identifier (string name)) *> op "=" *> term <* some nl
  pure (Binding (N name ::: ty) body)
  <?> "binding"

dataType =
  Datatype <$> ((:::) <$ keyword "data" <*> constructorName <* colon <*> type')
           <*> braces (constructor `sepBy` semi)
           <?> "datatype"
  where constructor = (:::) <$> constructorName <* colon <*> type' <?> "constructor"


nl, nls :: TokenParsing m => m ()

nl = () <$ newline <?> "newline"
nls = () <$ many (token newline <?> "newline")


term, application, true, false, var, let', lambda, tuple, case' :: (Monad m, TokenParsing m) => m (Term.Term Name)

-- | Parse a term.
term = application

application = atom `chainl1` pure (Term.#) <?> "function application"
  where atom = choice [ true, false, var, let', lambda, tuple, case' ]

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
  where pattern = name <|> I (-1) <$ token (string "_") <?> "pattern"

-- $
-- >>> parseString tuple "()"
-- Right (Term {unTerm = Intro Unit})
-- >>> parseString tuple "(True)"
-- Right (Term {unTerm = Intro (Bool True)})
-- >>> parseString tuple "(True, False)"
-- Right (Term {unTerm = Intro (Pair (Term {unTerm = Intro (Bool True)}) (Term {unTerm = Intro (Bool False)}))})
-- >>> parseString tuple "((), True, False)"
-- Right (Term {unTerm = Intro (Pair (Term {unTerm = Intro (Pair (Term {unTerm = Intro Unit}) (Term {unTerm = Intro (Bool True)}))}) (Term {unTerm = Intro (Bool False)}))})
tuple = parens (chainl1 (nls *> term <* nls) (Term.pair <$ nls <* comma <* nls) <|> pure Term.unit) <?> "tuple"

case' = Term.case' <$ keyword "case" <*> term <* keyword "of" <* nls <*> braces (((,) <$> pattern <* nls <* op "->" <* nls <*> term) `sepBy` (nls <* semi <* nls))

pattern :: (Monad m, TokenParsing m) => m Pattern
pattern = Variable <$> name
      <|> (Wildcard <$ token (string "_") <?> "_")
      <|> (Pattern.Constructor <$> constructorName <*> pure [] <?> "nullary data constructor")
      <|> parens (Pattern.Constructor <$> constructorName <*> many pattern <?> "n-ary data constructor")


type', piType, product, typeApplication, boolT, typeT, typeC, tvar :: (Monad m, TokenParsing m) => m (Type.Type Name)

type' = piType

piType = ((Type..->) <$> parens constraint <* op "->" <*> piType <?> "dependent function type")
  <|> (makePi <$> product <*> optional (op "->" *> piType) <?> "function type")
  where makePi ty1 Nothing = ty1
        makePi ty1 (Just ty2) = I (-1) ::: ty1 Type..-> ty2
        constraint = (:::) <$> name <* colon <*> type'

product = typeApplication `chainl1` ((Type..*) <$ symbolic '*') <?> "product type"

typeApplication = atom `chainl1` pure (Type.#) <?> "type application"
  where atom = choice [ boolT, typeT, typeC, tvar ]

-- $
-- >>> parseString boolT "Bool"
-- Right (Type {unType = Intro BoolT})
boolT = Type.boolT <$ keyword "Bool"

-- $
-- >>> parseString typeT "Type"
-- Right (Type {unType = Intro TypeT})
typeT = Type.typeT <$ keyword "Type"

typeC = Type.typeC <$> constructorName <*> many type' <?> "type constructor"

tvar = Type.tvar <$> name <?> "type variable"


name, constructorName, moduleName :: (Monad m, TokenParsing m) => m Name

name = N <$> identifier <?> "name"
constructorName = N <$> typeIdentifier <?> "constructor name"

moduleName = token (runUnspaced name') <?> "module name"
  where name' = makeN <$> typeIdentifier <*> optional (dot *> name')
        makeN s Nothing  = N s
        makeN s (Just n) = Q s n

identifier, typeIdentifier :: (Monad m, TokenParsing m) => m String

identifier     = ident (IdentifierStyle "identifier" lower alphaNum reservedWords Identifier ReservedIdentifier) <?> "identifier"
typeIdentifier = ident (IdentifierStyle "type identifier" upper alphaNum reservedWords Identifier ReservedIdentifier) <?> "type identifier"

reservedWords :: HashSet.HashSet String
reservedWords =  HashSet.fromList [ "exl", "exr", "let", "in", "module", "where", "import", "data", "case", "of" ]

keyword, op :: TokenParsing m => String -> m String

keyword s = token (highlight ReservedIdentifier (try (string s <* notFollowedBy alphaNum))) <?> s

op s = token (highlight Operator (string s)) <?> s
