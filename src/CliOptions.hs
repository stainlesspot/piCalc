module CliOptions
  ( Options(..)
  , opts
  , optsInfo
  ) where

import Parameters
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
  { precision :: Integer
  , threadLimit :: ThreadLimit
  , granularity :: Int
  , outputFile :: String
  , quiet :: Bool
  }

opts :: Parser Options
opts
  = Options
  <$> optPrecision
  <*> optThreadLimit
  <*> optGranularity
  <*> optOutputFile
  <*> optQuiet

-- | Full parser with info, used in @Main.main@
optsInfo :: ParserInfo Options
optsInfo = info (helper <*> opts)
  $  fullDesc
  <> progDesc "calculate pi with parallelization"
  <> footer "Repository: <https://github.com/stainlesspot/piCalc>"

-- | Number of digits of pi to be calculated
optPrecision :: Parser Integer
optPrecision = option auto
  $  short 'p'
  <> long "precision"
  <> metavar "N"
  <> help "Set the number of digits of pi to be calculated (required)"

optThreadLimit :: Parser ThreadLimit
optThreadLimit = option (maybeReader readThreadLimit)
  $  short 't'
  <> long "thread-limit"
  <> metavar "N"
  <> help "Limit the number of threads used up to N. Set to 0 for unlimited number of threads, up to machine's maximum ammount."
  <> value UnlimitedThreads
  <> showDefaultWith (const "unlimited")

optGranularity :: Parser Int
optGranularity = option auto
  $  short 'g'
  <> long "granularity"
  <> metavar "N"
  <> help "Set granularity of the parallelization"
  <> value defaultGranularity
  <> showDefault

optOutputFile :: Parser String
optOutputFile = strOption
  $  short 'o'
  <> long "output-file"
  <> metavar "FILE"
  <> help "Output calculated digits to FILE"
  <> value "_"

optQuiet :: Parser Bool
optQuiet = switch
  $  short 'q'
  <> long "quiet"
  <> help "Prevent output of messages to stdout"

