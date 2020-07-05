import Paths_piCalc (getDataFileName)
import PiCalc (showFixed, fromPrecision, calcPi)
import Data.Number.Fixed (dynamicEps)
import qualified Data.Number.FixedFunctions as F
import Sqrt2 (sqrt2)
import Test.Hspec
import Data.Number.Fixed

import Debug.Trace

main :: IO ()
main = do
  spiMil <- readFile =<< getDataFileName "data/pi.million"
  hspec $ do
    describe "sqrt 2" $ do
      sqrt2WithDigitsSpec 5
      sqrt2WithDigitsSpec 11
      sqrt2WithDigitsSpec 12
      sqrt2WithDigitsSpec 15
      sqrt2WithDigitsSpec 100
      sqrt2WithDigitsSpec 2003
      sqrt2WithDigitsSpec 2999
      sqrt2WithDigitsSpec 10000
      sqrt2WithDigitsSpec 30000

    describe "pi" $ do
      piWithDigitsSpec spiMil 5
      piWithDigitsSpec spiMil 11
      piWithDigitsSpec spiMil 110
      piWithDigitsSpec spiMil 800
      piWithDigitsSpec spiMil 1000
      piWithDigitsSpec spiMil 10000
      piWithDigitsSpec spiMil 30000

matchesLibraryPi :: Rational -> Integer -> IO ()
matchesLibraryPi pi' prec = dynamicEps eps (shouldBe $ fromRational pi') piLP
  where
    eps = fromPrecision prec
    piLP = F.pi eps

matchesThirdPartyPi :: Rational -> Integer -> String -> IO ()
matchesThirdPartyPi pi' prec spiTP = spi' `shouldBe` spiTP
  where
    spi' = showFixed prec pi'

piWithDigitsSpec :: String -> Integer -> Spec
piWithDigitsSpec spiM prec 
  = context ("with " ++ show prec ++ " digits") $ do
    it "matches library-provided pi" $
      matchesLibraryPi pi' prec
    it "matches third-party pi" $
      matchesThirdPartyPi pi' prec spiTP
  where
    pi' = calcPi prec
    spiTP = take (fromIntegral prec + 2) spiM

sqrt2WithDigitsSpec :: Integer -> Spec
sqrt2WithDigitsSpec prec
  = context ("with " ++ show prec ++ " digits") $ do
    it "matches library-provided sqrt2" $
      --dynamicEps eps (shouldBe $ fromRational (F.sqrt eps 2)) (sqrt2 eps)
      --dynamicEps eps (shouldBe $ fromRational (F.sqrt eps 2)) (sqrt2 eps)
      dynamicEps eps show diff `shouldSatisfy` \_ ->
          abs diff < (eps * 10)
  where
    eps = fromPrecision prec
    diff = sqrt2 eps - (F.sqrt eps 2)
