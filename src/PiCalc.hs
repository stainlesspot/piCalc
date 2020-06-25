{-# LANGUAGE BangPatterns #-}

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


-- | create Multipliers, starting from @k@
-- when @k == 0@ we have the initial multipliers,
-- which are different in @mL@ and @mH@
multipliers :: Integer -> Multipliers
multipliers k = Multipliers
  { mK = k
  , mL = l
  , mX = 1
  , mH = h
  , mM = 1
  } where 
    l = if k == 0 then 1103 else 0 
    h = if k == 0 then -2 else 0

increase :: Multipliers -> Multipliers
increase (Multipliers k l x h m) = Multipliers
  { mK = k'
  , mL = l + 26390
  , mX = x * 24591257856 
  , mH = h'
  , mM = m * 4 * (((h' ^ 3) - h') % (k' ^ 3))
  } where
    k' = k + 1
    h' = h + 4

combine :: Multipliers -> Multipliers -> Multipliers
combine (Multipliers k l x h m) (Multipliers k' l' x' h' m')
  = Multipliers
  { mK = k + k'
  , mL = l + l'
  , mX = x * x'
  , mH = h + h'
  , mM = m * m'
  }




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

    numTerms = ceiling (p / digitsPerTerm)
    sum = calcPartialSum numTerms


-- | Main target for parallelization
calcPartialSum :: Integer -> Rational
calcPartialSum numTerms = go (multipliers 0) 0
  where
    go :: Multipliers -> Rational -> Rational
    go !mult !res
      | mK mult == numTerms = res
      | otherwise           = go (increase mult) (res + term mult)

-- | Sums terms from i to j in the series
--calcRangeSum :: (Integer,Integer) -> Rational
--calcRangeSum (i,j) = 

showFixed :: Integer -> Rational -> String
showFixed prec rat = case ds of
  []      -> qs
  (_:ds') -> qs ++ '.' : take (fromIntegral prec) ds'
  where
    (qs,ds) = break (== '.') str
    eps = fromPrecision $ prec + ceiling (fromIntegral prec / 1000)
    str = dynamicEps eps show (approxRational rat eps)


getMult :: Integer -> Multipliers
getMult n = go (multipliers 0)
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
