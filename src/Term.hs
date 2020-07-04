module Term where

import Data.Ratio ((%))
import Control.DeepSeq (NFData(..))

-- Represents a term in the partial sum of pi
data Term = Term
  { mK :: Integer
  , mL :: Integer
  , mX :: Integer
  , mH :: Integer
  , mM :: Rational
  } deriving (Show, Eq)

instance NFData Term where
  rnf (Term k l x h m) = k `seq` l `seq` x `seq` h `seq` m `seq` ()


initial :: Term
initial = Term
  { mK = 0
  , mL = 1103
  , mX = 1
  , mH = -2
  , mM = 1
  }

term :: Integer -> Term
term k
  | k == 0    = initial
  | otherwise = Term
    { mK = k
    , mL = 26390
    , mX = 24591257856 
    , mH = h
    , mM = 4 * (((h ^ 3) - h) % (k ^ 3))
    } where
      h = 4 * k - 2

---- | Neutral with respect to @combine@
----
---- neutral `combine` t == t `combine` neutral == t
neutral :: Term
neutral = Term
  { mK = 0
  , mL = 0
  , mX = 1
  , mH = 0
  , mM = 1
  }

increase :: Term -> Term
increase (Term !k !l !x !h !m) = Term
  { mK = k'
  , mL = l + 26390
  , mX = x * 24591257856 
  , mH = h'
  , mM = m * 4 * (((h' ^ 3) - h') % (k' ^ 3))
  } where
    k' = k + 1
    h' = h + 4

combine :: Term -> Term -> Term
combine (Term k l x h m) (Term k' l' x' h' m')
  = Term
  { mK = max k k'
  , mL = l + l'
  , mX = x * x'
  , mH = if k > k' then h else h'
  , mM = m * m'
  }

calcTerm :: Term -> Rational
calcTerm (Term _ mL mX _ mM) = mM * (mL % mX)
