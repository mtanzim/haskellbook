{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TooMany where

-- TODO: revisit this!
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, s) = tooMany n && s == "much"

instance TooMany (Int, Int) where
  tooMany (n, n') = tooMany (n + n')

newtype NumTooMany a = NumTooMany (a, a)

instance (Num a, TooMany a) => TooMany (NumTooMany a) where
  tooMany (NumTooMany (x, x')) = tooMany x && tooMany x'

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
