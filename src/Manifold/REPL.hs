{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, TypeOperators #-}
module Manifold.REPL where

import Control.Applicative (Alternative(..))
import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import Data.Functor (($>))
import Data.Semiring
import Manifold.Name.Annotated
import Manifold.Context
import Manifold.Eval
import Manifold.Name
import Manifold.Pretty
import Manifold.Prompt
import Manifold.Proof
import Manifold.Proof.Checking
import Manifold.Purpose
import Manifold.Parser as Parser
import Manifold.Substitution
import Manifold.Term
import Manifold.Type
import Manifold.Value
import Text.Trifecta as Trifecta

repl :: (Members '[Prompt, REPL usage] effects, Monoid usage, Pretty usage) => Proof usage effects ()
repl = prompt >>= maybe repl handleInput

handleInput :: (Members '[Prompt, REPL usage] effects, Monoid usage, Pretty usage) => String -> Proof usage effects ()
handleInput str = case Parser.parseString command str of
  Left err -> output err *> repl
  Right action -> action

command :: (Members '[Prompt, REPL usage] effects, Monoid usage, Pretty usage) => Parser.Parser Trifecta.Parser (Proof usage effects ())
command = whole (meta <|> eval <$> term) <?> "command"
  where meta = colon
          *> ((long "help" <|> short 'h' <|> short '?' <?> "help") $> (sendREPL Help *> repl)
          <|> (long "quit" <|> short 'q' <?> "quit") $> pure ()
          <|> (typeOf <$> ((long "type" <|> short 't') *> term) <?> "type of")
          <?> "command; use :? for help")
        eval term = sendREPL (Eval term) >>= output . either pretty pretty >> repl
        typeOf term = sendREPL (TypeOf term) >>= output . either pretty pretty >> repl

        short = symbol . (:[])
        long = symbol


sendREPL :: Member (REPL usage) effects => REPL usage result -> Proof usage effects result
sendREPL = send

data REPL usage result where
  Help :: REPL usage ()
  TypeOf :: Term Name -> REPL usage (Either (Error (Annotated usage)) (Type (Annotated usage)))
  Eval :: Term Name -> REPL usage (Either (Error (Annotated usage)) Value)

runREPL :: (Eq usage, Member Prompt effects, Monoid usage, Unital usage) => Proof usage (REPL usage ': effects) a -> Proof usage effects a
runREPL = interpret (\case
  Help -> output (unlines
    [ ":help, :h, :?     - print this help text"
    , ":quit, :q         - exit the REPL"
    , ":type, :t <expr>  - print the type of <expr>"
    ])
  TypeOf term -> runCheck Intensional (infer term)
  Eval term -> runCheck Intensional (infer term) >>= either (pure . Left) (const (Right <$> runEval (eval term))))


runCheck :: Purpose -> Proof usage (Reader (Context (Annotated usage) (Type (Annotated usage))) ': Reader Purpose ': Fresh ': State (Substitution (Type (Annotated usage))) ': Exc (Error (Annotated usage)) ': effects) (Type (Annotated usage)) -> Proof usage effects (Either (Error (Annotated usage)) (Type (Annotated usage)))
runCheck purpose = runError . runSubstitution . runFresh 0 . runReader purpose . runContext

runEval :: Proof usage (Reader (Context Name Value) ': effects) a -> Proof usage effects a
runEval = runEnv

runIO :: (Eq usage, Monoid usage, Unital usage) => Proof usage '[REPL usage, Prompt] a -> IO a
runIO = runPrompt "λ: " . runREPL
