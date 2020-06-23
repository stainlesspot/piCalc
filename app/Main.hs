module Main (main) where

import CliOptions (optsInfo)
import Options.Applicative (execParser)

main :: IO ()
main = do
  options <- execParser optsInfo
  pure ()
