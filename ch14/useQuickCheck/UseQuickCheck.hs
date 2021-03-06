module UseQuickCheck where

import Data.Char (toUpper)
import Data.List (sort)
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

-- TODO: missed a few exercises here

capitalizeWord :: String -> String
capitalizeWord (first : rest) = toUpper first : rest
capitalizeWord [] = []

twice f = f . f

fourTimes = twice . twice

f x = (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)

fCommon x = (sort x == twice sort x) && (sort x == fourTimes sort x)

f' :: [Int] -> Bool
f' = fCommon

fChar :: [Char] -> Bool
fChar = fCommon

runIdempotence = quickCheck f

runIdempotence' = quickCheck f'

runIdempotenceChar = quickCheck fChar

reverseId :: String -> Bool
reverseId s = (==) ((reverse . reverse) s) (id s)

runReverseId = quickCheck reverseId

foldrCons :: [Char] -> [Char] -> Bool
foldrCons cs cs' = foldr (:) cs cs' == (++) cs cs'

runFoldrCons = quickCheck foldrCons

foldrConcat :: [[Char]] -> Bool
foldrConcat cs = foldr (++) [] cs == concat cs

runFoldrConcat = quickCheck foldrConcat

takeLength :: Int -> [Char] -> Bool
takeLength n xs = length (take n xs) == n

runTakeLength = quickCheck takeLength

readShow x = (read (show x)) == x

runReadShow = quickCheck (readShow :: Double -> Bool)

data Fool = Fulse | Frue deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

foolGen' :: Gen Fool
foolGen' = frequency [(3, return Fulse), (2, return Frue)]

main :: IO ()
main = hspec $ do
  describe "half" $ do
    it "half works" $ do
      property $ \x -> (half' x) == (x :: Double)