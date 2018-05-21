{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, TypeOperators #-}
module Manifold.REPL where

import Control.Monad.Effect
import Manifold.Expr
import System.Console.Haskeline

data REPL usage result where
  Help :: REPL usage ()
  TypeOf :: Term usage -> REPL usage (Type usage)
  Quit :: REPL usage ()


prompt :: Member Prompt effects => Eff effects (Maybe String)
prompt = send Prompt

output :: Member Prompt effects => String -> Eff effects ()
output = send . Output

data Prompt result where
  Prompt :: Prompt (Maybe String)
  Output :: String -> Prompt ()

runPrompt :: String -> Eff '[Prompt] a -> IO a
runPrompt prompt action = do
  prefs <- readPrefs "~/.local/Manifold/repl_prefs"
  runInputTWithPrefs prefs settings (runM (reinterpret (\case
    Prompt -> sendInputT (getInputLine (cyan <> prompt <> plain))
    Output s -> sendInputT (outputStrLn s)) action))

cyan :: String
cyan = "\ESC[1;36m\STX"

plain :: String
plain = "\ESC[0m\STX"

sendInputT :: Member (InputT IO) effects => InputT IO a -> Eff effects a
sendInputT = send

settings :: Settings IO
settings = Settings
  { complete = noCompletion
  , historyFile = Just "~/.local/Manifold/repl_history"
  , autoAddHistory = True
  }
