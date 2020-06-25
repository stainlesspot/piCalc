module PiCalc
  ( calcPi
  , showFixed
  , getMult
  , term
  , fromPrecision
  ) where

import Data.Ratio ((%), approxRational)
import qualified Data.Number.FixedFunctions as F (sqrt, approx)
import Data.Number.Fixed
  ( Fixed
  , Epsilon
  , dynamicEps
  , convertFixed
  , Eps1
  , EpsDiv10
  , precision
  , with_added_precision
  )

data Multipliers = Multipliers
  { mK :: Integer
  , mL :: Integer
  , mX :: Integer
  , mH :: Integer
  , mM :: Rational
  } deriving (Show, Eq)

initialMult :: Multipliers
initialMult = Multipliers
  { mK = 0
  , mL = 1103
  , mX = 1
  , mH = -2
  , mM = 1
  }

increase :: Multipliers -> Multipliers
increase (Multipliers mK' mL' mX' mH' mM') = Multipliers
  { mK = mK
  , mL = mL' + 26390
  , mX = mX' * 24591257856 
  , mH = mH
  , mM = mM' * 4 * (((mH ^ 3) - mH) % (mK ^ 3))
  } where
    mK = mK' + 1
    mH = mH' + 4

term :: Multipliers -> Rational
term (Multipliers _ mL mX _ mM) = mM * (mL % mX)

-- Approximate number of digits of pi each term gives
-- https://rmmc.eas.asu.edu/rmj/rmjVOLS2/vol19/vol19-1/bor.pdf (page 94)
digitsPerTerm :: Num a => a
digitsPerTerm = 7

calcPi :: Integer -> Rational
calcPi prec = approxRational (mC / sum) (eps / 1000)
  where
    p = fromIntegral prec
    eps = fromPrecision $ prec + ceiling (p / 1000)
    mC = 9801 / (2 * F.sqrt eps 2)
    sum = go initialMult 0
    numTerms = ceiling (p / digitsPerTerm)
    go :: Multipliers -> Rational -> Rational
    go mult res
      | mK mult == numTerms = res
      | otherwise           = go (increase mult) (res + term mult)

showFixed :: Integer -> Rational -> String
showFixed prec rat = case ds of
  []      -> qs
  (_:ds') -> qs ++ '.' : take (fromIntegral prec) ds'
  where
    (qs,ds) = break (== '.') str
    eps = fromPrecision $ prec + ceiling (fromIntegral prec / 1000)
    str = dynamicEps eps show (approxRational rat eps)


getMult :: Integer -> Multipliers
getMult n = go initialMult
  where
    go mult
      | mK mult == n = mult
      | otherwise    = go $ increase mult

-- | Used in parBuffer to indicate the number of sparks to be created initially.
-- value taken from:
-- https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch03.html#sec_parBuffer
sparkBufferSize :: Int
sparkBufferSize = 100

fromPrecision :: Integer -> Rational
fromPrecision p = 1 % (10 ^ p)
