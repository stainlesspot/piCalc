module Sqrt2 (sqrt2) where

import Data.Ratio (approxRational, (%))
import Data.List (foldl', genericTake)
import Data.Bifunctor (Bifunctor(bimap))
import Control.Parallel.Strategies (parBuffer, withStrategy, parList, rdeepseq)

data Sqrt2Term = STerm
  { sK :: Integer
  , sM :: Rational
  }

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

sqrt2 :: Rational -> Rational
sqrt2 eps
  = go 1 0
  $ withStrategy (parBuffer 100 rdeepseq)
  $ map (termsSumLast . sqrt2RangeTerms)
  $ ranges
  where
    n = 100
    ranges = iterate (bimap (+n) (+n)) (0,n)
    go :: Rational -> Rational -> [(Rational, Rational)] -> Rational
    go !l !v ((s,l'):ps)
      | abs ls < eps' = approxRational v' eps'
      | otherwise    = go (l * l') v' ps 
      where
        ls = l * s
        v' = v + ls
        eps' = eps / 10
