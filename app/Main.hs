module Main (main) where

import CliOptions (Options(..), optsInfo)
import Options.Applicative (execParser)
import PiCalc (calcPi, showFixed, fromPrecision)
import qualified Data.Number.FixedFunctions as F
import Data.Number.Fixed (dynamicEps)
import Data.Function (on)
import Paths_piCalc (getDataFileName)
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

  let eps = fromPrecision p
      pi'' = (F.pi eps)
      spi'' = showFixed p pi'

  validPi <- readFile =<< getDataFileName "data/pi.million"
  
  let valid1 = dynamicEps eps (== fromRational pi'') pi'
      spi''' = take (fromIntegral p + 2) validPi
      valid2 = spi''' == spi'
      message1 =
        if valid1
           then "calculation matches library-provided pi"
           else "calculation does not match library-provided pi\n" ++ spi''
      message2 = 
        if valid2
           then "calculation matches third-party pi records (more accurate)"
           else "calculation does not match third-party pi records (more accurate)\n" ++ spi'''

  writeFile outputFile spi'

  unless quiet $ do
    putStrLn "=========="
    putStrLn message1
    putStrLn "=========="
    putStrLn message2
    putStrLn "=========="
    printf "calculation time: %.3fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
