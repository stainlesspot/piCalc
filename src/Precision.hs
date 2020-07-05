module Precision where

import Data.Ratio (approxRational, (%))
import Data.Number.Fixed (dynamicEps)

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
