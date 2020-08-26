module PiCalc
  ( calcPi
  , calcPiP
  , sparkBufferSize
  , chunkRange
  ) where

import Parameters
import Term
import Sqrt2 (calcSqrt2)
import Data.Ratio ((%), approxRational)
import Data.List (foldl', iterate', genericTake)
import Control.Parallel.Strategies
  ( rdeepseq
  , withStrategy
  , parBuffer
  , parList
  )
import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.Number.FixedFunctions as F



---- | return the sum of all terms in the list and the last term
--termsSumLast :: [Term] -> (Rational, Term)
--termsSumLast = foldl' add (0, neutral)
--  where
--    add :: (Rational, Term) -> Term -> (Rational, Term)
--    add (s, _) t = (s', t)
--      where
--        s' = s + calcTerm t
--
---- | get terms in the range [i,j) - j is excluded
--piRangeTerms :: (Integer, Integer) -> [Term]
--piRangeTerms (i, j)
--  = genericTake (max 0 (j-i))
--  $ iterate' increase
--  $ term i
--
-- Sum terms in the range (i,j). return the sum and the last term
termsSumLast :: (Integer, Integer) -> (Rational, Term)
termsSumLast (i, j) = go i 0 $ term i
  where
    go !i' !s !t
      | i' >= j = (s, t)
      | otherwise = go (i'+1) (s + calcTerm t) (increase t)

  
--partialSum :: Parameters -> Rational
--partialSum params@Params{ numThreads = nt, granularity = g }
--  = go 1 0
--  $ withStrategy (parList rdeepseq)
--  $ map (termsSumLast . piRangeTerms)
--  $ chunkRange len (0, n)
--  where
--    n = numTerms params
--    len = max 1 (n `div` fromIntegral (nt * g))
--    go :: Rational -> Rational -> [(Rational, Term)] -> Rational
--    go _  !v [] = v
--    go !q !v ((s,t):ps) = go q' v' ps
--      where
--        v' = v + s * q
--        q' = q * tM t

partialSum :: Parameters -> Rational
partialSum params@Params{ numThreads = nt, granularity = g }
  = go 1 0
  -- $ withStrategy (parList rdeepseq)
  $ map termsSumLast
  $ chunkRange nt (0, n)
  where
    n = numTerms params

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
    sqrt2 = 1 --toRational $ sqrt 2
    sum = partialSum params
    
calcPiP :: Parameters -> Rational
calcPiP params = pi'
  where
    pi' = 9801 / (2 * sqrt2 * sum)
    sqrt2 = calcSqrt2 params
    sum = partialSum params
