{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, TypeOperators #-}
module Manifold.REPL where

import Control.Monad.Effect
import Manifold.Expr
import Manifold.Proof
import Manifold.Type.Checking
import System.Console.Haskeline

sendREPL :: Member (REPL usage) effects => REPL usage result -> Proof usage effects result
sendREPL = send

data REPL usage result where
  Help :: REPL usage ()
  TypeOf :: Term usage -> REPL usage (Type usage)

runREPL :: Members '[Check usage, Prompt] effects => Proof usage (REPL usage ': effects) a -> Proof usage effects a
runREPL = relay pure (\ repl yield -> case repl of
  Help -> output (unlines
    [ ":help, :h, :?     - print this help text"
    , ":quit, :q         - exit the REPL"
    , ":type, :t <expr>  - print the type of <expr>"
    ]) >>= yield
  TypeOf term -> infer term >>= yield)


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
