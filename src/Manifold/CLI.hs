{-# LANGUAGE TypeApplications #-}
module Manifold.CLI where

import Control.Monad ((>=>))
import Control.Effect
import Data.List (intersperse)
import Data.Semilattice.Lower
import Data.Semiring.Class
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

argumentsParser :: Prelude (Annotated ()) -> ParserInfo (IO ())
argumentsParser prelude = info
  (version <*> helper <*> options prelude)
  (  fullDesc
  <> progDesc "Manifold is a small experiment in quantitative type theory."
  <> header   "Manifold - a quantitative, dependently-typed language")
  where options prelude = flag' (runIO prelude repl) (short 'i' <> long "interactive" <> help "run in interactive mode (REPL)")
              <|> (runFile prelude >=> prettify) <$> some (strArgument (metavar "FILES" <> help "The files to check."))

runFile :: (Eq usage, Monoid usage, Unital usage) => Prelude (Annotated usage) -> [FilePath] -> IO (Either (ProofError (Annotated usage)) (ModuleTable (Annotated usage) (Term Name), [Module (Annotated usage) (Term Name)]))
runFile _ paths = do
  ms <- traverse (parseFile (whole module') >=> maybe exitFailure pure) paths
  pure (run
       (runError
       (runReader (fromModules ms)
       (runState (lowerBound @(ModuleTable (Annotated _) (Term Name)))
       (runProof
       (traverse checkModule ms))))))

prettify :: Pretty usage => Either (ProofError (Annotated usage)) (ModuleTable (Annotated usage) (Term Name), [Module (Annotated usage) (Term Name)]) -> IO ()
prettify = either
  (prettyPrint >=> const exitFailure)
  (putDoc . vsep . intersperse mempty . map pretty . snd)


versionString :: String
versionString = "Manifold version " <> showVersion Library.version

version :: Options.Parser (a -> a)
version = infoOption versionString (long "version" <> short 'V' <> help "Output version info.")
