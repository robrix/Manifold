{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, TypeOperators #-}
module Manifold.Prompt where

import Control.Monad.Effect
import System.Console.Haskeline

prompt :: (Effectful m, Member Prompt effects) => m effects (Maybe String)
prompt = send Prompt

output :: (Effectful m, Member Prompt effects) => String -> m effects ()
output = send . Output

data Prompt result where
  Prompt :: Prompt (Maybe String)
  Output :: String -> Prompt ()

runPrompt :: Effectful m => String -> m '[Prompt] a -> IO a
runPrompt prompt action = runInputT settings (runM (reinterpret (\case
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
