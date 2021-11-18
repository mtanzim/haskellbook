module UseQuickCheck where

import Test.Hspec
import Test.QuickCheck

half x = x / 2

half' = (* 2) . half

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Int -> Bool
plusCommutative x y z = x + (y + z) == (x + y) + z

runQcPlusAssociative :: IO ()
runQcPlusAssociative = quickCheck plusAssociative

runQcPlusCommutative :: IO ()
runQcPlusCommutative = quickCheck plusCommutative

main :: IO ()
main = hspec $ do
  describe "half" $ do
    it "half works" $ do
      property $ \x -> (half' x) == (x :: Double)