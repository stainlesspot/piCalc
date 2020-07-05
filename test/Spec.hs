import Precision
import qualified Parameters as P (precision)
import Parameters
import Paths_piCalc (getDataFileName)
import PiCalc (calcPi)
import Data.Number.Fixed (dynamicEps)
import qualified Data.Number.FixedFunctions as F
import Sqrt2 (calcSqrt2)
import Test.Hspec
import Data.Number.Fixed

import Control.Concurrent (getNumCapabilities)

main :: IO ()
main = do
  spiMil <- readFile =<< getDataFileName "data/pi.million"
  t <- getNumCapabilities
  hspec $ do
    describe "sqrt 2" $ do
      sqrt2WithDigitsSpec $ parameters t 5
      sqrt2WithDigitsSpec $ parameters t 11
      sqrt2WithDigitsSpec $ parameters t 12
      sqrt2WithDigitsSpec $ parameters t 15
      sqrt2WithDigitsSpec $ parameters t 100
      sqrt2WithDigitsSpec $ parameters t 2003
      sqrt2WithDigitsSpec $ parameters t 2999
      sqrt2WithDigitsSpec $ parameters t 10000
      sqrt2WithDigitsSpec $ parameters t 30000

    describe "pi" $ do
      piWithDigitsSpec spiMil $ parameters t 5
      piWithDigitsSpec spiMil $ parameters t 11
      piWithDigitsSpec spiMil $ parameters t 110
      piWithDigitsSpec spiMil $ parameters t 800
      piWithDigitsSpec spiMil $ parameters t 1000
      piWithDigitsSpec spiMil $ parameters t 10000
      piWithDigitsSpec spiMil $ parameters t 30000

parameters :: Int -> Integer -> Parameters
parameters t p = Params
  { P.precision = p
  , granularity = 3
  , numThreads = t
  }

matchesLibraryPi :: Rational -> Integer -> IO ()
matchesLibraryPi pi' prec = dynamicEps eps (shouldBe $ fromRational pi') piLP
  where
    eps = fromPrecision prec
    piLP = F.pi eps

matchesThirdPartyPi :: Rational -> Integer -> String -> IO ()
matchesThirdPartyPi pi' prec spiTP = spi' `shouldBe` spiTP
  where
    spi' = showFixed prec pi'

piWithDigitsSpec :: String -> Parameters -> Spec
piWithDigitsSpec spiM params 
  = context ("with " ++ show prec ++ " digits") $ do
      it "matches library-provided pi" $
        matchesLibraryPi pi' prec
      it "matches third-party pi" $
        matchesThirdPartyPi pi' prec spiTP
  where
    prec = P.precision params
    pi' = calcPi params
    spiTP = take (fromIntegral prec + 2) spiM

sqrt2WithDigitsSpec :: Parameters -> Spec
sqrt2WithDigitsSpec params
  = context ("with " ++ show prec ++ " digits") $ do
    it "matches library-provided sqrt2" $
      (dynamicEps eps show diff, sqrt2, sqrt2') `shouldSatisfy` \_ ->
          abs diff < (eps * 10)
  where
    prec = P.precision params
    eps = fromPrecision prec
    sqrt2 = calcSqrt2 params
    sqrt2' = F.sqrt eps 2
    diff = sqrt2 - sqrt2'
