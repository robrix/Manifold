{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, KindSignatures, LambdaCase, TypeOperators #-}
module Manifold.Prompt where

import Control.Monad.Effect
import System.Console.Haskeline

prompt :: (Effectful m, Member Prompt effects) => m effects (Maybe String)
prompt = send Prompt

output :: (Effectful m, Member Prompt effects) => String -> m effects ()
output = send . Output

data Prompt (m :: * -> *) result where
  Prompt :: Prompt m (Maybe String)
  Output :: String -> Prompt m ()

runPrompt :: Effectful m => String -> m '[Prompt] a -> IO a
runPrompt prompt action = runInputT settings (runM (reinterpret (\case
  Prompt -> sendInputT (getInputLine (cyan <> prompt <> plain))
  Output s -> sendInputT (outputStrLn s)) action))

cyan :: String
cyan = "\ESC[1;36m\STX"

plain :: String
plain = "\ESC[0m\STX"

sendInputT :: (Effectful m, Member (Lift (InputT IO)) effects) => InputT IO a -> m effects a
sendInputT = send . Lift

settings :: Settings IO
settings = Settings
  { complete = noCompletion
  , historyFile = Just "~/.local/Manifold/repl_history"
  , autoAddHistory = True
  }


instance PureEffect Prompt
instance Effect Prompt where
  handleState c dist (Request Prompt k) = Request Prompt (dist . (<$ c) . k)
  handleState c dist (Request (Output s) k) = Request (Output s) (dist . (<$ c) . k)
