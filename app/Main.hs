module Main where

import Control.Monad
import Manifold.CLI
import Options.Applicative

main :: IO ()
main = join (execParser argumentsParser)
