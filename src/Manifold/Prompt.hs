{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Manifold.Prompt where

import Control.Effect
import System.Console.Haskeline

prompt :: (Member Prompt sig, Carrier sig m) => m (Maybe String)
prompt = send (Prompt gen)

output :: (Member Prompt sig, Carrier sig m) => String -> m ()
output = send . flip Output (gen ())

data Prompt m k
  = Prompt (Maybe String -> k)
  | Output String k
  deriving (Functor)

runPrompt :: String -> Eff (PromptC (Eff (LiftC (InputT IO)))) a -> IO a
runPrompt prompt = runInputT settings . runM . flip runPromptC prompt . interpret

newtype PromptC m a = PromptC { runPromptC :: String -> m a }

instance (Carrier (Lift (InputT IO)) m, Monad m) => Carrier Prompt (PromptC m) where
  gen a = PromptC (\ _ -> pure a)
  alg (Prompt k) = PromptC (\ prompt -> sendInputT (getInputLine (cyan <> prompt <> plain)) >>= flip runPromptC prompt . k)
  alg (Output s k) = PromptC (\ prompt -> sendInputT (outputStrLn s) *> runPromptC k prompt)

cyan :: String
cyan = "\ESC[1;36m\STX"

plain :: String
plain = "\ESC[0m\STX"

sendInputT :: (Member (Lift (InputT IO)) sig, Carrier sig m) => InputT IO a -> m a
sendInputT = send . Lift . fmap gen

settings :: Settings IO
settings = Settings
  { complete = noCompletion
  , historyFile = Just "~/.local/Manifold/repl_history"
  , autoAddHistory = True
  }


instance HFunctor Prompt where
  hfmap _ (Prompt k) = Prompt k
  hfmap _ (Output s k) = Output s k

instance Effect Prompt where
  handle state handler (Prompt k) = Prompt (handler . (<$ state) . k)
  handle state handler (Output s k) = Output s (handler (k <$ state))
