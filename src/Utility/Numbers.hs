module Utility.Numbers
  ( Positive
  , fromPositive
  , toPositive

  , NonNegative
  , fromNonNegative
  , toNonNegative
  ) where

newtype Positive = Positive Int
  deriving (Show, Eq)

fromPositive :: Positive -> Int
fromPositive (Positive n) = n

toPositive :: Int -> Maybe Positive
toPositive n =
  if n > 0
     then Just $ Positive n
     else Nothing


newtype NonNegative = NonNegative Int
  deriving (Show, Eq)

fromNonNegative :: NonNegative -> Int
fromNonNegative (NonNegative n) = n

toNonNegative :: Int -> Maybe NonNegative
toNonNegative n =
  if n < 0
     then Nothing
     else Just $ NonNegative n
