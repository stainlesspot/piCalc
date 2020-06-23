module CliOptions
  ( Options
  , opts
  , optsInfo
  ) where

import Options.Applicative
  ( Parser
  , ParserInfo
  , info
  , helper
  , fullDesc
  , progDesc
  , footer
  , short
  , long
  , metavar
  , value
  , help
  , switch
  , strOption
  , option
  , auto
  , maybeReader
  , showDefault
  , showDefaultWith
  )

import ThreadLimit (ThreadLimit(..), readThreadLimit)

data Options = Options
  { optNumTerms :: Int
  , optThreadLimit :: ThreadLimit
  , optOutputFile :: String
  , optQuiet :: Bool
  }

opts :: Parser Options
opts = Options <$> numTerms <*> threadLimit <*> outputFile <*> quiet

-- | Full parser with info, used in @Main.main@
optsInfo :: ParserInfo Options
optsInfo = info (helper <*> opts)
  $  fullDesc
  <> progDesc "calculate pi with parallelization"
  <> footer "Repository: <https://github.com/stainlesspot/piCalc>"

-- | Number of terms of the partial sum to be calculated
numTerms :: Parser Int
numTerms = option auto
  $  short 'p'
  <> long "num-terms"
  <> metavar "N"
  <> help "Set the number of terms in the partial sum to be calculated (required)"

threadLimit :: Parser ThreadLimit
threadLimit = option (maybeReader readThreadLimit)
  $  short 't'
  <> long "thread-limit"
  <> metavar "N"
  <> help "Limit the number of threads used up to N. Set to 0 for unlimited number of threads, up to machine's maximum ammount."
  <> value UnlimitedThreads
  <> showDefaultWith (const "unlimited")

outputFile :: Parser String
outputFile = strOption
  $  short 'o'
  <> long "output-file"
  <> metavar "FILE"
  <> help "Output calculated digits to FILE"
  <> value "pi.out"
  <> showDefault

quiet :: Parser Bool
quiet = switch
  $  short 'q'
  <> long "quiet"
  <> help "Prevent output of messages to stdout"

