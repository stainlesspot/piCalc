module CheckPi
  (printPiCheck) where

import PiCalc (showFixed, fromPrecision)
import Paths_piCalc (getDataFileName)
import qualified Data.Number.FixedFunctions as F (pi)
import Data.Number.Fixed (dynamicEps)

printPiCheck :: Integer -> Rational -> String -> IO ()
printPiCheck prec pi' spi' = do
  piMillionDigits <- readFile =<< getDataFileName "data/pi.million"

  let eps = fromPrecision prec
      pi'' = (F.pi eps)
      spi'' = showFixed prec pi''
      spi''' = take (fromIntegral prec + 2) piMillionDigits

      isValid1 = dynamicEps eps (== fromRational pi'') pi'
      isValid2 = spi''' == spi'

      msg1 =
        if isValid1
           then "calculation matches library-provided pi"
           else spi'' ++ "\ncalculation does not match library-provided pi"
      msg2 = 
        if isValid2
           then "calculation matches third-party pi records (more accurate)"
           else spi''' ++
                "\ncalculation does not match third-party pi records (more accurate)"
  putStrLn "=========="
  putStrLn msg1
  putStrLn "=========="
  putStrLn msg2
  putStrLn "=========="
