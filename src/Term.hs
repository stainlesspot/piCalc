module Term where

import Data.Ratio ((%))
import Control.DeepSeq (NFData(..))

-- Represents a term in the partial sum of pi
data Term = Term
  { tK :: Integer
  , tL :: Integer
  , tM :: Rational
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
    , tM = h * (h+1) * (h+2) % (k^3 * 6147814464)
    } where
      h = 4 * k - 1

---- | Neutral with respect to @combine@
----
---- neutral `combine` t == t `combine` neutral == t
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
  , tM = m * 4 * (h * (h+1) * (h+2) %  (k'^3 * 6147814464))
  } where
    k' = k + 1
    h  = 4 * k - 1

--combine :: Term -> Term -> Term
--combine (Term k l x h m) (Term k' l' x' h' m')
--  = Term
--  { mK = max k k'
--  , mL = l + l'
--  , mX = x * x'
--  , mH = if k > k' then h else h'
--  , mM = m * m'
--  }

calcTerm :: Term -> Rational
calcTerm (Term _ l m) = m * (l % 1)
