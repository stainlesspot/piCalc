{-# LANGUAGE DeriveGeneric #-}
module Term where

import qualified Data.Ratio as R 
import Control.DeepSeq (NFData(..))
import Debug.Trace (traceShowId)
import Control.Applicative (Applicative(liftA2))
import Data.Function (on)
import GHC.Real (reduce, Ratio((:%)))
import GHC.Generics (Generic)

--type Rat = R.Rational
newtype Rat = Rat Rational
  deriving (Eq, Ord, Show, Generic)

instance Num Rat where
    (Rat (x:%y)) + (Rat (x':%y')) = Rat $ reduce (x*y' + x'*y) (y*y')
    (Rat (x:%y)) - (Rat (x':%y')) = Rat $ reduce (x*y' - x'*y) (y*y')
    (Rat (x:%y)) * (Rat (x':%y')) = Rat $ reduce (x * x') (y * y')
    negate (Rat (x:%y)) =  Rat $ (-x) :% y
    abs (Rat (x:%y))    =  Rat $ abs x :% y
    signum (Rat (x:%_)) =  Rat $ signum x :% 1
    fromInteger x       =  Rat $ fromInteger x :% 1

instance NFData Rat where

(%) :: Integer -> Integer -> Rat
x % y = Rat $ x R.% y --(/) `on` fromInteger

infixl 7 %

instance Fractional Rat where
  (Rat (x:%y)) / (Rat (x':%y'))   =  (x*y') % (y*x')
  recip (Rat (0:%_))        = undefined -- ratioZeroDenominatorError
  recip (Rat (x:%y))
      | x < 0         = Rat $ negate y :% negate x
      | otherwise     = Rat $ y :% x
  fromRational (x:%y) =  fromInteger x % fromInteger y

-- Represents a term in the partial sum of pi
data Term = Term
  { tK :: !Integer
  , tL :: !Integer
  --, tM :: !Rat
  , tM :: !Integer
  , tX :: !Integer
  } deriving (Show, Eq)

instance NFData Term where
  rnf (Term k l m x) = k `seq` l `seq` m `seq` x `seq` ()


initial :: Term
initial = Term
  { tK = 0
  , tL = 1103
  , tM = 1
  , tX = 1
  }

term :: Integer -> Term
term 0 = initial
term k = Term
    { tK = k
    , tL = 1103 + 26390 * k
    --, tM = h * (h-1) * (h-2) % (k^3 * 6147814464)
    , tM = h * (h-1) * (h-2)
    , tX = k^3 * 6147814464
    } where
      h = 4 * k - 1

neutral :: Term
neutral = Term
  { tK = 0
  , tL = 0
  , tM = 1
  , tX = 1
  }

increase :: Term -> Term
increase (Term k l m x) = Term
  { tK = k'
  , tL = l + 26390
  , tM = m * (partM `quot` d)
  , tX = x * (partX `quot` d)
  } where
    k' = k + 1
    h  = 4 * k' - 1
    partM = h * (h-1) * (h-2)
    partX = k'^3 * 6147814464 -- 2^6 * 3^8 * 11^4
    d = gcd partM partX

calcTerm :: Term -> Rat
calcTerm (Term _ l m x) = (m * l) % x

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
