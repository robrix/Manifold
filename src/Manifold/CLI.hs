{-# LANGUAGE TypeApplications #-}
module Manifold.CLI where

import Control.Monad ((>=>))
import Control.Monad.Effect
import Data.Version (showVersion)
import Manifold.Eval
import Manifold.Parser
import Manifold.Purpose
import Manifold.REPL
import Manifold.Type.Checking
import Options.Applicative as Options
import qualified Paths_Manifold as Library (version)
import System.Exit (exitFailure)

argumentsParser :: ParserInfo (IO ())
argumentsParser = info
  (version <*> helper <*> options)
    (fullDesc
  <> progDesc "Manifold is a small experiment in quantitative type theory."
  <> header   "Manifold - a quantitative, dependently-typed language")
  where options = flag' (runIO @() repl) (short 'i' <> long "interactive" <> help "run in interactive mode (REPL)")
              <|> runFile <$> strArgument (metavar "FILE" <> help "The program to run.")

runFile :: FilePath -> IO ()
runFile path = do
  term <- parseFile (whole term) path >>= maybe exitFailure pure
  _    <- either (print >=> const exitFailure) pure (run (runCheck Intensional (infer @() term)))
  print (run (runEval (eval term)))


versionString :: String
versionString = "Manifold version " <> showVersion Library.version

version :: Options.Parser (a -> a)
version = infoOption versionString (long "version" <> short 'V' <> help "Output version info.")
