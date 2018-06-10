{-# LANGUAGE TypeApplications #-}
module Manifold.CLI where

import Control.Monad ((>=>))
import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.List (intersperse)
import Data.Semilattice.Lower
import Data.Version (showVersion)
import Manifold.Module
import Manifold.Name
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

argumentsParser :: Prelude () -> ParserInfo (IO ())
argumentsParser prelude = info
  (version <*> helper <*> options prelude)
    (fullDesc
  <> progDesc "Manifold is a small experiment in quantitative type theory."
  <> header   "Manifold - a quantitative, dependently-typed language")
  where options prelude = flag' (runIO prelude repl) (short 'i' <> long "interactive" <> help "run in interactive mode (REPL)")
              <|> runFile prelude <$> some (strArgument (metavar "FILES" <> help "The files to check."))

runFile :: Prelude () -> [FilePath] -> IO ()
runFile _ paths = do
  ms <- traverse (parseFile (whole module') >=> maybe exitFailure pure) paths
  either (prettyPrint @(Error (Annotated ())) >=> const exitFailure)
         (putDoc . vsep . intersperse mempty . map pretty)
         (run (runError
              (runReader (fromModules ms)
              (fmap fst
              (runState (lowerBound @(ModuleTable (Annotated ()) (Term Name)))
              (traverse (checkModule @()) ms))))))


versionString :: String
versionString = "Manifold version " <> showVersion Library.version

version :: Options.Parser (a -> a)
version = infoOption versionString (long "version" <> short 'V' <> help "Output version info.")
