module Term where

import Data.Ratio ((%))
import Control.DeepSeq (NFData(..))
import Debug.Trace (traceShowId)

-- Represents a term in the partial sum of pi
data Term = Term
  { tK :: !Integer
  , tL :: !Integer
  , tM :: !Rational
  } deriving (Show, Eq)

instance NFData Term where
  rnf (Term k l m) = k `seq` l `seq` m `seq` ()


initial :: Term
initial = Term
  { tK = 0
  , tL = 1103
  , tM = 1
  }

term :: Integer -> Term
term 0 = initial
term k = Term
    { tK = k
    , tL = 1103 + 26390 * k
    , tM = h * (h-1) * (h-2) % (k^3 * 6147814464)
    } where
      h = 4 * k - 1

neutral :: Term
neutral = Term
  { tK = 0
  , tL = 0
  , tM = 1
  }

increase :: Term -> Term
increase (Term !k !l !m) = Term
  { tK = k'
  , tL = l + 26390
  , tM = m * (h * (h-1) * (h-2) % (k'^3 * 6147814464))
  } where
    k' = k + 1
    h  = 4 * k' - 1

calcTerm :: Term -> Rational
calcTerm (Term _ l m) = m * (l % 1)

---- | chunk a range into subranges each with a length of @n@
--chunkRange :: Integer -> (Integer, Integer) -> [(Integer, Integer)]
--chunkRange l (i, j)
--  | i >= j    = []
--  | otherwise = let !i' = min j (i+l)
--                 in (i, i') : chunkRange l (i', j)
--
-- @n@ = number of chunks
chunkRange :: Integral a => a -> (Integer, Integer) -> [(Integer, Integer)]
chunkRange n (l, h) = go l h
  where !len = max 1 $ ceiling $ fromInteger (h-l) / fromIntegral n
        go !i !j
          | i >= j    = []
          | otherwise = let !i' = min j (i+len)
                         in (i, i') : go i' j


-- | Used in parBuffer to indicate the number of sparks to be created initially.
-- value taken from:
-- https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch03.html#sec_parBuffer
sparkBufferSize :: Int
sparkBufferSize = 32
