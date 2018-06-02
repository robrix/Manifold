{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving, TypeFamilies #-}
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

import Control.Applicative (Alternative(..), (<**>))
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Function (on)
import qualified Data.HashSet as HashSet
import Data.List (deleteBy)
import Manifold.Constraint
import Manifold.Declaration
import Manifold.Module (Module(Module))
import Manifold.Name (Name(..))
import Manifold.Pattern as Pattern
import Manifold.Pretty (prettyShow)
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
type MonadParsing m = (IndentationParsing m, Monad m, TokenParsing m)

newtype Inner a = Inner { runInner :: Trifecta.Parser a }
  deriving (Alternative, Applicative, CharParsing, DeltaParsing, Functor, LookAheadParsing, MarkParsing Delta, Monad, MonadPlus, Parsing)

instance TokenParsing Inner where
  someSpace = Inner $ buildSomeSpaceParser (skipSome (satisfy isSpace)) haskellCommentStyle
  nesting = Inner . nesting . runInner
  highlight h = Inner . highlight h . runInner

parseFile :: MonadIO m => Parser a -> FilePath -> m (Maybe a)
parseFile p = parseFromFile (runInner (whiteSpace *> evalCharIndentationParserT p indentst))

parseString :: Parser a -> String -> Either String a
parseString p = toResult . Trifecta.parseString (runInner (evalCharIndentationParserT p indentst)) directed

toResult :: Trifecta.Result a -> Either String a
toResult r = case r of
  Trifecta.Success a -> Right a
  Trifecta.Failure info -> Left (show (Trifecta._errDoc info))


evalCharIndentationParserT :: Monad m => IndentationParserT Char m a -> IndentationState -> m a
evalCharIndentationParserT = evalIndentationParserT

-- evalTokenIndentationParserT :: Monad m => IndentationParserT Token m a -> IndentationState -> m a
-- evalTokenIndentationParserT = evalIndentationParserT

directed = Directed BS.empty 0 0 0 0
indentst = mkIndentationState 0 infIndentation True Gt


whole p = whiteSpace *> p <* eof
whole :: TokenParsing m => m a -> m a


module' :: MonadParsing m => m (Module Name (Term.Term Name))
module' =
  Module <$ keyword "module" <*> moduleName <* keyword "where"
         <*> imports
         <*> declarations
  where imports = many (absoluteIndentation import')
        declarations = many (absoluteIndentation declaration) >>= resolve
        resolve decls = checkDecls [] $ foldr combine [] decls
        combine (Done d) rest = ((declarationName d, Done d) : rest)
        combine (Sig name ty) rest = case lookup name rest of
          Just (Bind name tm) -> (name, Done (Binding (name ::: ty) tm)) : deleteBy ((==) `on` fst) (name, Bind name tm) rest
          _ -> (name, Sig name ty) : rest
        combine (Bind name tm) rest = case lookup name rest of
          Just (Sig name ty) -> (name, Done (Binding (name ::: ty) tm)) : deleteBy ((==) `on` fst) (name, Sig name ty) rest
          _ -> (name, Bind name tm) : rest
        checkDecls names ((name, Done d) : ds)
          | name `notElem` names = (d :) <$> checkDecls (name:names) ds
          | otherwise = fail ("redundant definition of " <> prettyShow name)
        checkDecls _ ((name, Sig _ _) : _) = fail ("no definition for " <> prettyShow name)
        checkDecls _ ((name, Bind _ _) : _) = fail ("no signature for " <> prettyShow name)
        checkDecls _ [] = pure []

data Decl
  = Sig Name (Type.Type Name)
  | Bind Name (Term.Term Name)
  | Done (Declaration Name (Term.Term Name))


import' :: (Monad m, TokenParsing m) => m Name
import' = keyword "import" *> moduleName

declaration, datatype :: MonadParsing m => m Decl
signature, binding :: MonadParsing m => m (Name -> Decl)

declaration = choice [ name <**> (signature <|> binding), datatype ]

signature = flip Sig <$ colon <*> type' <?> "type signature"

binding = flip Bind <$ op "=" <*> term <?> "binding"

datatype = fmap Done $
  Datatype <$> ((:::) <$ keyword "data" <*> constructorName <* colon <*> type')
           <*> option [] (keyword "where" *> gconstructors)
           <?> "datatype"
  where gconstructors =
          (   braces (constructor `sepBy` semi)
          <|> localIndentation Gt (many (absoluteIndentation constructor)))
        constructor = (:::) <$> constructorName <* colon <*> type' <?> "constructor"


term, application, true, false, var, data', let', lambda, tuple, case' :: MonadParsing m => m (Term.Term Name)

-- | Parse a term.
term = application

application = atom `chainl1` pure (Term.#) <?> "function application"
  where atom = choice [ true, false, var, data', let', lambda, tuple, case' ]

-- $
-- >>> parseString true "True"
-- Right (Term {unTerm = Value (Bool True)})
true = Term.true <$ keyword "True"

-- $
-- >>> parseString false "False"
-- Right (Term {unTerm = Value (Bool False)})
false = Term.false <$ keyword "False"

var = Term.var <$> name <?> "variable"

data' = Term.var <$> constructorName <?> "data constructor"

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
-- Right (Term {unTerm = Value Unit})
-- >>> parseString tuple "(True)"
-- Right (Term {unTerm = Value (Bool True)})
-- >>> parseString tuple "(True, False)"
-- Right (Term {unTerm = Value (Pair (Term {unTerm = Value (Bool True)}) (Term {unTerm = Value (Bool False)}))})
-- >>> parseString tuple "((), True, False)"
-- Right (Term {unTerm = Value (Pair (Term {unTerm = Value (Pair (Term {unTerm = Value Unit}) (Term {unTerm = Value (Bool True)}))}) (Term {unTerm = Value (Bool False)}))})
tuple = parens (term `chainl1` (Term.pair <$ comma) <|> pure Term.unit) <?> "tuple"

case' = Term.case' <$ keyword "case" <*> term <* keyword "of" <*>
  (   braces (match `sepBy` semi)
  <|> localIndentation Gt (many (absoluteIndentation match)))
  where match = (,) <$> pattern <* op "->" <*> term

pattern :: (Monad m, TokenParsing m) => m Pattern
pattern = (Variable <$> name <?> "binding pattern")
      <|> (Wildcard <$ token (string "_") <?> "wildcard pattern")
      <|> (Pattern.Constructor <$> constructorName <*> pure [] <?> "nullary data constructor pattern")
      <|> (parens (pattern' `chainl1` (Pattern.pair <$ comma) <|> pure Pattern.unit) <?> "tuple pattern")
  where pattern' = pattern <|> (Pattern.Constructor <$> constructorName <*> many pattern <?> "n-ary data constructor pattern")


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
-- Right (Type {unType = IntroT BoolT})
boolT = Type.boolT <$ keyword "Bool"

-- $
-- >>> parseString typeT "Type"
-- Right (Type {unType = IntroT TypeT})
typeT = Type.typeT <$ keyword "Type"

typeC = Type.tvar <$> constructorName <?> "type constructor"

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
