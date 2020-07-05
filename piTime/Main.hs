module Main (main) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Environment (getArgs)
import Text.Printf (printf)
import PiCalc (calcPi)

-- calculate pi outputing only total calc time
main :: IO ()
main = do
  [prec] <- getArgs

  let pi' = calcPi $ read prec
  
  t0 <- getCurrentTime
  t1 <- pi' `seq` getCurrentTime

  printf "calculation time: %.3fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
