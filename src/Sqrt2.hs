module Sqrt2 (calcSqrt2) where

import Term (chunkRange)
import Parameters
import Data.Ratio (approxRational, (%))
import Data.List (foldl', genericTake)
import Data.Bifunctor (Bifunctor(bimap))
import Control.Parallel.Strategies (parBuffer, withStrategy, parList, rdeepseq)

data Sqrt2Term = STerm
  { sK :: Integer
  , sM :: Rational
  } deriving (Show, Eq)

increase :: Sqrt2Term -> Sqrt2Term
increase (STerm k m) = STerm
  { sK = k'
  , sM = m * ((2*k' + 1) % (4 * k'))
  }  where
    k' = k + 1

initial :: Sqrt2Term
initial = STerm
  { sK = 0
  , sM = 1 % 2
  }

term :: Integer -> Sqrt2Term
term 0 = initial
term k = STerm
    { sK = k
    , sM = (2*k + 1) % (4 * k)
    }

sqrt2RangeTerms :: (Integer, Integer) -> [Sqrt2Term]
sqrt2RangeTerms (i, j)
  = genericTake (max 0 (j-i))
  $ iterate increase
  $ term i

-- | returns sum of the elements and the last element
termsSumLast :: [Sqrt2Term] -> (Rational, Rational)
termsSumLast
  = foldl' add (0, 0)
  . map sM
  where
    add :: (Rational, Rational) -> Rational -> (Rational, Rational)
    add (s, _) m = (s + m, m)

--calcSqrt2 :: Parameters -> Rational
--calcSqrt2 params@Params{ numThreads = nt, granularity = g }
--  = go 1 0
--  $ withStrategy (parBuffer 100 rdeepseq)
--  $ map (termsSumLast . sqrt2RangeTerms)
--  $ ranges
--  where
--    eps = getEps params / 10
--    len = max 1 (numTerms params `div` fromIntegral (nt * g))
--    ranges = iterate (bimap (+ len) (+ len)) (0, len)
--    go :: Rational -> Rational -> [(Rational, Rational)] -> Rational
--    go !l !v ((s,l'):ps)
--      | abs ls < eps = approxRational v' eps
--      | otherwise    = go (l * l') v' ps 
--      where
--        ls = l * s
--        v' = v + ls
--
calcSqrt2 :: Parameters -> Rational
calcSqrt2 params@Params{ numThreads = nt, granularity = g }
  = go 1 0
  $ withStrategy (parList rdeepseq)
  $ map (termsSumLast . sqrt2RangeTerms)
  $ chunkRange len (0, n)
  where
    n = numTerms params
    len = max 1 (n `div` fromIntegral (nt * g))
    go :: Rational -> Rational -> [(Rational, Rational)] -> Rational
    go _  !v [] = v
    go !q !v ((s,l):ps) = go q' v' ps
      where
        v' = v + s * q
        q' = q * l

