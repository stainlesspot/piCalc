module Main (main) where

import Precision
import qualified Parameters as P
import ThreadLimit
import Utility.Numbers
import CliOptions (Options(..), optsInfo)
import PiCalc (calcPi)
import Options.Applicative (execParser)
import Data.Function (on)
import Control.Monad (unless)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import Control.Concurrent (getNumCapabilities, setNumCapabilities)


main :: IO ()
main = do
  Options
    { precision = p
    , threadLimit = tl
    , granularity = g
    , outputFile = outputFile
    , quiet = quiet
    } <- execParser optsInfo

  t <- case tl of
    UnlimitedThreads -> getNumCapabilities
    NumThreads nonneg -> do
      let n = fromNonNegative nonneg
      setNumCapabilities n
      return n

  let pi' = calcPi P.Params{
        P.precision = p
      , P.granularity = g
      , P.numThreads = t
      }
  
  t0 <- getCurrentTime
  t1 <- pi' `seq` getCurrentTime
  
  unless (outputFile == "_") $ do
    writeFile outputFile $ showFixed p pi'

  unless quiet $ do
    printf "calculation time: %.3fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
    actualNumThreads <- getNumCapabilities
    printf "with %d %d threads\n" actualNumThreads t
