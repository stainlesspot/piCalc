module Main (main) where

import CliOptions (Options(..), optsInfo)
import PiCalc (calcPi, showFixed)
import CheckPi (printPiCheck)
import Options.Applicative (execParser)
import Data.Function (on)
import Control.Monad (unless)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import Control.DeepSeq (deepseq)

main :: IO ()
main = do
  Options
    { precision = p
    , quiet = quiet
    , outputFile = outputFile
    } <- execParser optsInfo
  let pi' = calcPi p
      spi' = showFixed p pi'
  
  t0 <- getCurrentTime 
  t1 <- spi' `deepseq` getCurrentTime 


  writeFile outputFile spi'

  unless quiet $ do
    printPiCheck p pi' spi'
    printf "calculation time: %.3fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
