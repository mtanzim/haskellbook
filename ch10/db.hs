module Ch10DB where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate
      ( UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123)
      ),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate
      ( UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123)
      )
  ]

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where
    f a b = case a of
      DbNumber n -> b ++ [n]
      _ -> b

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where
    f a b = case a of
      DbDate utcTime -> b ++ [utcTime]
      _ -> b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent ds = foldr f (head (filterDbDate ds)) ds
  where
    f a b = case a of
      DbDate utcTime -> max b utcTime
      _ -> b