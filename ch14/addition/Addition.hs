module Addition where

import Data.Int (Int16, Int32, Int8)
import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = do
  putStrLn "hello world"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

mult :: (Eq a, Num a) => a -> a -> a
mult a b
  | b == 1 = a
  | otherwise = a + mult a (b - 1)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, GT, EQ]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

main :: IO ()
main = hspec $ do
  describe "addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      (2 + 2) `shouldBe` 4
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
    it "2 times 8 should be 16" $ do
      mult 2 8 `shouldBe` 16
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
    it "my mult actually works for tiny positive numbers but it stack overflows for big numbers :(" $ do
      property $ \x x' -> mult (abs (x)) (abs (x')) == abs (x :: Int16) * abs (x' :: Int16)