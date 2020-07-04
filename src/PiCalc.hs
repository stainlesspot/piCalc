module PiCalc
  ( calcPiSeq
  , calcPiPar
  , showFixed
 -- , getMult
  , term
  , fromPrecision
  ) where


import Prelude hiding (pi)
import Term
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

-- Approximate number of digits of pi each term gives
-- https://rmmc.eas.asu.edu/rmj/rmjVOLS2/vol19/vol19-1/bor.pdf (page 94)
digitsPerTerm :: Num a => a
digitsPerTerm = 7

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

--getMult :: Integer -> Term
--getMult n = go initial
--  where
--    go !t
--      | mK t == n = t
--      | otherwise = go $ increase t

-- | Used in parBuffer to indicate the number of sparks to be created initially.
-- value taken from:
-- https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch03.html#sec_parBuffer
sparkBufferSize :: Int
sparkBufferSize = 100

calcPiWith :: (Integer -> Rational) -> Integer -> Rational
calcPiWith calcPS prec = approxRational (mC / sum) (eps / 1000)
  where
    p = fromIntegral prec
    eps = fromPrecision $ prec + ceiling (p / 1000)
    mC = 9801 / (2 * F.sqrt eps 2)

    numTerms = ceiling (p / digitsPerTerm)
    sum = calcPS numTerms


-- | get terms in the range [i,j) - j is excluded
-- also return the last term in the range, to be used for the next range
rangeTerms :: (Integer, Integer) -> [Term]
rangeTerms (i, j)
  = genericTake (max 0 (j-i))
  $ iterate' increase
  $ term i
  --where
   -- sumMapLast :: Num b => (a -> b) -> NonEmpty a -> (b, a)
   -- sumMapLast f (x:|xs) = go x xs (f x)
   --   where
   --     go last []     !acc = (acc, last)
   --     go _    (y:ys) !acc = go y ys (acc + f y)

-- | Used to fold [[Term]]
-- @l@ is the last term from the previous call to @addTermsSum@,
-- it is used to pass the initial values, and accumulated ones
-- to the other terms
addTermsSum :: (Rational, Term) -> [Term] -> (Rational, Term)
addTermsSum (!s, l) []     = (s, l)
addTermsSum (!s, l) (t:ts) = foldl' add (s + calcTerm y, y) ys
  where
    y = combine l t
    ys = map (combine l) ts
    add :: (Rational, Term) -> Term -> (Rational, Term)
    add (!acc, _) x = (acc + calcTerm x, x)

combineSums :: (Rational, Term) -> (Rational, Term) -> (Rational, Term)
combineSums (sum1, l1) (sum2, l2) =
  ( sum1 + (sum2 * calcTerm l1)
  , combine l1 l2
  )

chunkRange :: Integer -> (Integer, Integer) -> [(Integer, Integer)]
chunkRange n (!i, j)
  | i >= j    = []
  | otherwise = let i' = min j (i+n)
                 in (i, i') : chunkRange n (i', j)

calcPiPar :: Integer -> Rational
calcPiPar = calcPiWith calcPartialSumPar
  
calcPartialSumPar :: Integer -> Rational
calcPartialSumPar numTerms
  = fst
  $ foldl' addTermsSum (0, neutral)
  $ withStrategy (parList rdeepseq)--(parBuffer 100 rdeepseq)
  $ map rangeTerms
  $ chunkRange n (0, numTerms)
  where n = 100


calcPiSeq :: Integer -> Rational
calcPiSeq = calcPiWith calcPartialSumSeq

-- | Main target for parallelization
calcPartialSumSeq :: Integer -> Rational
calcPartialSumSeq numTerms = go initial 0
  where
    go :: Term -> Rational -> Rational
    go !t !res
      | mK t == numTerms = res
      | otherwise        = go (increase t) (res + calcTerm t)
