module ThreadLimit
  ( ThreadLimit(..)
  , readThreadLimit
  ) where

import Utility.Numbers (NonNegative, toNonNegative, fromNonNegative)
import Text.Read (readMaybe)

-- | Represents the input of the thread limit cli option
data ThreadLimit
  = UnlimitedThreads
  | NumThreads NonNegative
  deriving (Show, Eq)

toThreadLimit :: NonNegative -> ThreadLimit
toThreadLimit nn = case fromNonNegative nn of
  0 -> UnlimitedThreads
  _ -> NumThreads nn

readThreadLimit :: String -> Maybe ThreadLimit
readThreadLimit s
  = toThreadLimit <$> (toNonNegative =<< readMaybe s)
  
  

