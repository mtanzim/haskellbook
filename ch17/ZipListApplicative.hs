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
  ZipList' (f : fs) <*> ZipList' (l : ls) = ZipList' (f l : (fs <*> ls))
  (ZipList' []) <*> _ = ZipList' []
  _ <*> (ZipList' []) = ZipList' []

-- TODO: make sense of this solution:
-- https://github.com/andrewMacmurray/haskell-book-solutions/blob/f4fd386187c03828d1736d9a43642ab4f0ec6462/src/ch17/List.hs
-- main :: IO ()
-- main = quickBatch $ applicative (ZipList' [])

testMain :: ZipList' Integer
testMain =
  let zl' = ZipList'
      z = zl' [(+ 9), (* 2), (+ 8)]
      z' = zl' [1 .. 3]
   in z <*> z'