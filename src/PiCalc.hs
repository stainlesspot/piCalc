module PiCalc (calcPi) where

import Parameters
import Term
import Sqrt2 (calcSqrt2)
import Data.Ratio ((%), approxRational)
import Data.List (foldl', iterate', genericTake)
import Control.Parallel.Strategies
  ( rdeepseq
  , withStrategy
  , parBuffer
  )
import Data.Bifunctor (Bifunctor(bimap))


-- | Used in parBuffer to indicate the number of sparks to be created initially.
-- value taken from:
-- https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch03.html#sec_parBuffer
sparkBufferSize :: Int
sparkBufferSize = 100

-- | return the sum of all terms in the list and the last term
termsSumLast :: [Term] -> (Rational, Term)
termsSumLast = foldl' add (0, neutral)
  where
    add :: (Rational, Term) -> Term -> (Rational, Term)
    add (s, _) t = (s', t)
      where
        s' = s + calcTerm t

-- | get terms in the range [i,j) - j is excluded
piRangeTerms :: (Integer, Integer) -> [Term]
piRangeTerms (i, j)
  = genericTake (max 0 (j-i))
  $ iterate' increase
  $ term i

-- | chunk a range into subranges each with a length of @n@
chunkRange :: Integer -> (Integer, Integer) -> [(Integer, Integer)]
chunkRange l (i, j)
  | i >= j    = []
  | otherwise = let !i' = min j (i+l)
                 in (i, i') : chunkRange l (i', j)
  
partialSum :: Parameters -> Rational
partialSum params@Params{ numThreads = nt, granularity = g }
  = go 1 0
  $ withStrategy (parBuffer sparkBufferSize rdeepseq)
  $ map (termsSumLast . piRangeTerms)
  $ chunkRange len (0, n)
  where
    n = numTerms params
    len = max 1 (n `div` fromIntegral (nt * g))
    go :: Rational -> Rational -> [(Rational, Term)] -> Rational
    go _  !v [] = v
    go !q !v ((s,t):ps) = go q' v' ps
      where
        v' = v + s * q
        q' = q * tM t

calcPi :: Parameters -> Rational
calcPi params = pi'
  where
    pi' = 9801 / (2 * sqrt2 * sum)
    sqrt2 = calcSqrt2 params
    sum = partialSum params
