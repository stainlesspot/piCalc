module Main (main) where

import CliOptions (Options(..), optsInfo)
import PiCalc (calcPiPar, showFixed)
import Options.Applicative (execParser)
import Data.Function (on)
import Control.Monad (unless)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)


main :: IO ()
main = do
  Options
    { precision = p
    , quiet = quiet
    , outputFile = outputFile
    } <- execParser optsInfo

  let pi' = calcPiPar p
      spi' = showFixed p pi'
  
  t0 <- getCurrentTime
  t1 <- pi' `seq` getCurrentTime

  writeFile outputFile spi'

  unless quiet $ do
    printf "calculation time: %.3fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
