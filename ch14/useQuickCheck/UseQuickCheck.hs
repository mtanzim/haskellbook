module UseQuickCheck where

import Test.Hspec
import Test.QuickCheck

half x = x / 2

half' = (* 2) . half

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x

multAssociative :: Int -> Int -> Int -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: Int -> Int -> Bool
multCommutative x y = x * y == y * x

runQcPlusAssociative :: IO ()
runQcPlusAssociative = quickCheck plusAssociative

runQcPlusCommutative :: IO ()
runQcPlusCommutative = quickCheck plusCommutative

runQcMultAssociative :: IO ()
runQcMultAssociative = quickCheck multAssociative

runQcMultCommutative :: IO ()
runQcMultCommutative = quickCheck multCommutative

quotRemProp :: Gen Bool
quotRemProp = do
  x <- choose (1, 9999) :: Gen Integer
  y <- choose (1, 9999) :: Gen Integer
  return $ (quot x y) * y + (rem x y) == x

divModProp :: Gen Bool
divModProp = do
  x <- choose (1, 9999) :: Gen Integer
  y <- choose (1, 9999) :: Gen Integer
  return $ (div x y) * y + (mod x y) == x

runQuotRemProp = quickCheck quotRemProp

runDivModProp = quickCheck divModProp

expCommutative :: Int -> Int -> Int -> Bool
expCommutative x y z = x ^ (y ^ z) == (x ^ y) ^ z

expAssociative :: Int -> Int -> Bool
expAssociative x y = x ^ y == y ^ x

runExpCommutative :: IO ()
runExpCommutative = quickCheck expCommutative

runExpAssociative :: IO ()
runExpAssociative = quickCheck expAssociative

reverseProp :: String -> Bool
reverseProp s = (reverse . reverse) s == id s

runReverseProp = quickCheck reverseProp

fnProp :: (String -> Int) -> String -> Bool
fnProp f s = (f s) == (f $ s)

runFnProp = quickCheck (fnProp (\s -> length s))

main :: IO ()
main = hspec $ do
  describe "half" $ do
    it "half works" $ do
      property $ \x -> (half' x) == (x :: Double)