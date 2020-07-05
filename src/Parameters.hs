module Parameters where

import Precision

defaultGranularity :: Int
defaultGranularity = 100

-- Approximate number of digits each term gives
-- https://rmmc.eas.asu.edu/rmj/rmjVOLS2/vol19/vol19-1/bor.pdf (page 94)
digitsPerTerm :: Num a => a
digitsPerTerm = 7

data Parameters = Params
  { precision :: Integer -- number of digits of pi
  , granularity :: Int -- number of sparks to give to each thread
  , numThreads :: Int
  }

numTerms :: Parameters -> Integer
numTerms Params{ precision = p }
  = ceiling (fromInteger p / digitsPerTerm)

getEps :: Parameters -> Rational
getEps Params{ precision = p }
  = fromPrecision $ p + ceiling (fromInteger p / 1000)
