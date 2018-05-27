{-# LANGUAGE TypeApplications #-}
module Manifold.CLI where

import Control.Monad ((>=>))
import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Data.Version (showVersion)
import Manifold.Module
import Manifold.Name.Annotated
import Manifold.Parser
import Manifold.Pretty
import Manifold.Proof
import Manifold.Proof.Checking
import Manifold.REPL
import Manifold.Term
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
  m <- parseFile (whole module') path >>= maybe exitFailure pure
  either (print @(Error (Annotated ())) >=> const exitFailure) (prettyPrint @(Module (Annotated ()) Term)) (run (runError (runFresh 0 (checkModule @() m))))


versionString :: String
versionString = "Manifold version " <> showVersion Library.version

version :: Options.Parser (a -> a)
version = infoOption versionString (long "version" <> short 'V' <> help "Output version info.")
