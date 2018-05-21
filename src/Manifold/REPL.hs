{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, TypeOperators #-}
module Manifold.REPL where

import Control.Applicative (Alternative(..))
import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Data.Functor (($>))
import Data.Semiring (Semiring(..))
import Manifold.Context
import Manifold.Expr
import Manifold.Proof
import Manifold.Parser as Parser
import Manifold.Substitution
import Manifold.Type.Checking
import Manifold.Type.Formation
import System.Console.Haskeline
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

runREPL :: (Eq usage, Members '[CheckIsType usage, Exc (Error usage), Fresh, Prompt, Reader (Context usage), State (Substitution (Type usage))] effects, Monoid usage, Semiring usage) => Proof usage (REPL usage ': effects) a -> Proof usage effects a
runREPL = relay pure (\ repl yield -> case repl of
  Help -> output (unlines
    [ ":help, :h, :?     - print this help text"
    , ":quit, :q         - exit the REPL"
    , ":type, :t <expr>  - print the type of <expr>"
    ]) >>= yield
  TypeOf term -> (runCheck (Right <$> infer term) `catchError` (pure . Left)) >>= yield)


prompt :: (Effectful m, Member Prompt effects) => m effects (Maybe String)
prompt = send Prompt

output :: (Effectful m, Member Prompt effects) => String -> m effects ()
output = send . Output

data Prompt result where
  Prompt :: Prompt (Maybe String)
  Output :: String -> Prompt ()

runPrompt :: Effectful m => String -> m '[Prompt] a -> IO a
runPrompt prompt action = do
  prefs <- readPrefs "~/.local/Manifold/repl_prefs"
  runInputTWithPrefs prefs settings (runM (reinterpret (\case
    Prompt -> sendInputT (getInputLine (cyan <> prompt <> plain))
    Output s -> sendInputT (outputStrLn s)) action))

cyan :: String
cyan = "\ESC[1;36m\STX"

plain :: String
plain = "\ESC[0m\STX"

sendInputT :: (Effectful m, Member (InputT IO) effects) => InputT IO a -> m effects a
sendInputT = send

settings :: Settings IO
settings = Settings
  { complete = noCompletion
  , historyFile = Just "~/.local/Manifold/repl_history"
  , autoAddHistory = True
  }


runIO :: (Eq usage, Monoid usage, Semiring usage) => Proof usage '[REPL usage, CheckIsType usage, Reader (Context usage), Fresh, State (Substitution (Type usage)), Exc (Error usage), Prompt] a -> IO (Either (Error usage) (a, Substitution (Type usage)))
runIO = runPrompt "λ: " . runError . runSubstitution . runFresh 0 . runContext . runCheckIsType . runREPL
