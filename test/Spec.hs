import Paths_piCalc (getDataFileName)
import PiCalc (showFixed, fromPrecision, calcPiPar)
import Data.Number.Fixed (dynamicEps)
import qualified Data.Number.FixedFunctions as F (pi)
import Test.Hspec

main :: IO ()
main = do
  spiMil <- readFile =<< getDataFileName "data/pi.million"
  hspec $ describe "pi" $ do
    withDigitsSpec spiMil 5
    withDigitsSpec spiMil 10
    withDigitsSpec spiMil 1000
    withDigitsSpec spiMil 10000
    withDigitsSpec spiMil 30000

matchesLibraryPi :: Rational -> Integer -> Bool
matchesLibraryPi pi' prec = dynamicEps eps (== fromRational piLP) pi'
  where
    eps = fromPrecision prec
    piLP = F.pi eps

matchesThirdPartyPi :: Rational -> Integer -> String -> Bool
matchesThirdPartyPi pi' prec spiTP = spi' == spiTP
  where
    spi' = showFixed prec pi'

withDigitsSpec :: String -> Integer -> Spec
withDigitsSpec spiM prec 
  = context ("with " ++ show prec ++ " digits") $ do
    it "matches library-provided pi" $
      matchesLibraryPi pi' prec
    it "matches third-party pi" $
      matchesThirdPartyPi pi' prec spiTP
  where
    pi' = calcPiPar prec
    spiTP = take (fromIntegral prec + 2) spiM

