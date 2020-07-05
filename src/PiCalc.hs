module PiCalc
  ( calcPi
  , showFixed
 -- , getMult
  , fromPrecision
  ) where


import Prelude hiding (pi)
import Term
import Sqrt2 (sqrt2)
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
import Data.List.NonEmpty (fromList, NonEmpty(..))
import Data.List (foldl', iterate', genericTake)
import Control.Parallel.Strategies
  ( rdeepseq
  , withStrategy
  , parBuffer
  , parList
  )
import Data.Bifunctor (Bifunctor(bimap))


showFixed :: Integer -> Rational -> String
showFixed prec rat = case ds of
  []      -> qs
  (_:ds') -> qs ++ '.' : take (fromIntegral prec) ds'
  where
    (qs,ds) = break (== '.') str
    eps = fromPrecision $ prec + ceiling (fromIntegral prec / 1000)
    str = dynamicEps eps show (approxRational rat eps)

fromPrecision :: Integer -> Rational
fromPrecision p = 1 % (10 ^ p)

-- | Used in parBuffer to indicate the number of sparks to be created initially.
-- value taken from:
-- https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch03.html#sec_parBuffer
sparkBufferSize :: Int
sparkBufferSize = 100




---- | Used to fold [[Term]]
---- @l@ is the last term from the previous call to @addTermsSum@,
---- it is used to pass the initial values, and accumulated ones
---- to the other terms
--addTermsSum :: (Rational, Term) -> [Term] -> (Rational, Term)
--addTermsSum (!s, l) []     = (s, l)
--addTermsSum (!s, l) (t:ts) = foldl' add (s + calcTerm y, y) ys
--  where
--    y = combine l t
--    ys = map (combine l) ts
--    add :: (Rational, Term) -> Term -> (Rational, Term)
--    add (!acc, _) x = (acc + calcTerm x, x)

--combineSums :: (Rational, Term) -> (Rational, Term) -> (Rational, Term)
--combineSums (sum1, l1) (sum2, l2) =
--  ( sum1 + (sum2 * calcTerm l1)
--  , combine l1 l2
--  )
--
termsSumLast :: [Term] -> (Rational, Term)
termsSumLast
  = foldl' add (0, neutral)
  where
    add :: (Rational, Term) -> Term -> (Rational, Term)
    add (s, _) t = (s', t)
      where
        s' = s + calcTerm t

-- | get terms in the range [i,j) - j is excluded
-- also return the last term in the range, to be used for the next range
piRangeTerms :: (Integer, Integer) -> [Term]
piRangeTerms (i, j)
  = genericTake (max 0 (j-i))
  $ iterate' increase
  $ term i

chunkRange :: Integer -> (Integer, Integer) -> [(Integer, Integer)]
chunkRange n (!i, j)
  | i >= j    = []
  | otherwise = let i' = min j (i+n)
                 in (i, i') : chunkRange n (i', j)
  
partialSum :: Integer -> Rational
partialSum numTerms
  = go 1 0
  --  fst
  --  foldl' addTermsSum (0, neutral)
  --  withStrategy (parList rdeepseq)--(parBuffer 100 rdeepseq)
  $ map (termsSumLast . piRangeTerms)
  $ chunkRange n (0, numTerms)
  where
    n = 100
    go :: Rational -> Rational -> [(Rational, Term)] -> Rational
    go _  !v [] = v
    go !q !v ((s,t):ps) = go q' v' ps
      where
        v' = v + s * q
        q' = q * tM t
      

-- Approximate number of digits of pi each term gives
-- https://rmmc.eas.asu.edu/rmj/rmjVOLS2/vol19/vol19-1/bor.pdf (page 94)
digitsPerTerm :: Num a => a
digitsPerTerm = 7

calcPi :: Integer -> Rational
calcPi prec = approxRational pi' (eps / 1000)
  where
    p = fromIntegral prec
    eps = fromPrecision $ prec + ceiling (p / 1000)
    numTerms = ceiling (p / digitsPerTerm)

    sum = partialSum numTerms
    pi' = 9801 / (2 * sqrt2 eps * sum)
