{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, TypeOperators #-}
module Manifold.REPL where

import Control.Applicative (Alternative(..))
import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import Data.Functor (($>))
import Manifold.Binding
import Manifold.Context
import Manifold.Eval
import Manifold.Name
import Manifold.Prompt
import Manifold.Proof
import Manifold.Purpose
import Manifold.Parser as Parser
import Manifold.Substitution
import Manifold.Term
import Manifold.Type
import Manifold.Type.Checking
import Manifold.Value
import Text.Trifecta as Trifecta

repl :: (Members '[Prompt, REPL usage] effects, Monoid usage, Show usage) => Proof usage effects ()
repl = prompt >>= maybe repl handleInput

handleInput :: (Members '[Prompt, REPL usage] effects, Monoid usage, Show usage) => String -> Proof usage effects ()
handleInput str = case Parser.parseString command str of
  Left err -> output err *> repl
  Right action -> action

command :: (Members '[Prompt, REPL usage] effects, Monoid usage, Show usage) => Parser.Parser Trifecta.Parser (Proof usage effects ())
command = whole (meta <|> eval <$> term) <?> "command"
  where meta = colon
          *> ((long "help" <|> short 'h' <|> short '?' <?> "help") $> (sendREPL Help *> repl)
          <|> (long "quit" <|> short 'q' <?> "quit") $> pure ()
          <|> (typeOf <$> ((long "type" <|> short 't') *> term) <?> "type of")
          <?> "command; use :? for help")
        eval term = sendREPL (Eval term) >>= output . either show show >> repl
        typeOf term = sendREPL (TypeOf term) >>= output . either show show >> repl

        short = symbol . (:[])
        long = symbol


sendREPL :: Member (REPL usage) effects => REPL usage result -> Proof usage effects result
sendREPL = send

data REPL usage result where
  Help :: REPL usage ()
  TypeOf :: Term -> REPL usage (Either (Error (Binding usage)) (Type (Binding usage)))
  Eval :: Term -> REPL usage (Either (Error (Binding usage)) Value)

runREPL :: (Eq usage, Member Prompt effects, Monoid usage) => Proof usage (REPL usage ': effects) a -> Proof usage effects a
runREPL = interpret (\case
  Help -> output (unlines
    [ ":help, :h, :?     - print this help text"
    , ":quit, :q         - exit the REPL"
    , ":type, :t <expr>  - print the type of <expr>"
    ])
  TypeOf term -> fmap (uncurry (flip apply)) <$> runCheck Intensional (infer term)
  Eval term -> fmap (uncurry (flip apply)) <$> runCheck Intensional (infer term) >>= either (pure . Left) (const (Right <$> runEval (eval term))))


runCheck :: Purpose -> Proof usage (Reader (Context (Binding usage) (Type (Binding usage))) ': Reader Purpose ': Fresh ': State (Substitution (Type (Binding usage))) ': Exc (Error (Binding usage)) ': effects) a -> Proof usage effects (Either (Error (Binding usage)) (a, Substitution (Type (Binding usage))))
runCheck purpose = runError . runSubstitution . runFresh 0 . runReader purpose . runContext

runEval :: Proof usage (Reader (Context Name Value) ': effects) a -> Proof usage effects a
runEval = runContext

runIO :: (Eq usage, Monoid usage) => Proof usage '[REPL usage, Prompt] a -> IO a
runIO = runPrompt "λ: " . runREPL
