{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, TypeOperators #-}
module Manifold.REPL where

import Control.Applicative (Alternative(..))
import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Data.Functor (($>))
import Data.Semiring (Semiring(..))
import Manifold.Context
import Manifold.Expr
import Manifold.Prompt
import Manifold.Proof
import Manifold.Parser as Parser
import Manifold.Substitution
import Manifold.Type.Checking
import Text.Trifecta as Trifecta

repl :: (Members '[Prompt, REPL usage] effects, Monoid usage, Show usage) => Proof usage effects ()
repl = prompt >>= maybe repl handleInput

handleInput :: (Members '[Prompt, REPL usage] effects, Monoid usage, Show usage) => String -> Proof usage effects ()
handleInput str = case Parser.parseString command str of
  Left err -> output err *> repl
  Right action -> action

command :: (Members '[Prompt, REPL usage] effects, Monoid usage, Show usage) => Parser.Parser Trifecta.Parser (Proof usage effects ())
command = whole meta <?> "command"

meta :: (Members '[Prompt, REPL usage] effects, Monoid usage, Show usage) => Parser.Parser Trifecta.Parser (Proof usage effects ())
meta = colon
  *> ((long "help" <|> short 'h' <|> short '?' <?> "help") $> (sendREPL Help *> repl)
  <|> (long "quit" <|> short 'q' <?> "quit") $> pure ()
  <|> (typeOf <$> ((long "type" <|> short 't') *> term) <?> "type of")
  <?> "command; use :? for help")
  where typeOf term = sendREPL (TypeOf term) >>= output . either show show >> repl

short :: Char -> Parser.Parser Trifecta.Parser String
short = symbol . (:[])

long :: String -> Parser.Parser Trifecta.Parser String
long = symbol


sendREPL :: Member (REPL usage) effects => REPL usage result -> Proof usage effects result
sendREPL = send

data REPL usage result where
  Help :: REPL usage ()
  TypeOf :: Term usage -> REPL usage (Either (Error usage) (Type usage))

runREPL :: (Eq usage, Member Prompt effects, Monoid usage, Semiring usage) => Proof usage (REPL usage ': effects) a -> Proof usage effects a
runREPL = interpret (\case
  Help -> output (unlines
    [ ":help, :h, :?     - print this help text"
    , ":quit, :q         - exit the REPL"
    , ":type, :t <expr>  - print the type of <expr>"
    ])
  TypeOf term -> fmap (uncurry (flip apply)) <$> runCheck (infer term))


runCheck :: Proof usage (Reader (Context usage) ': Fresh ': State (Substitution (Type usage)) ': Exc (Error usage) ': effects) a -> Proof usage effects (Either (Error usage) (a, Substitution (Type usage)))
runCheck = runError . runSubstitution . runFresh 0 . runContext

runIO :: (Eq usage, Monoid usage, Semiring usage) => Proof usage '[REPL usage, Prompt] a -> IO a
runIO = runPrompt "λ: " . runREPL
