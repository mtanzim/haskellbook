module ZipListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take 3000 l
      ys' =
        let (ZipList' l) = ys
         in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure l = ZipList' [l]
--   (f : fs) <*> (l : ls) = fmap f l : (fs <*> ls)
  (ZipList' []) <*> _ = ZipList' []
  _ <*> (ZipList' []) = ZipList' []

-- main :: IO ()
-- main = quickBatch $ applicative (ZipList' 'a')