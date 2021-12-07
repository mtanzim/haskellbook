module Exercises where

a :: [Int]
a = fmap (+ 1) (read "[1]" :: [Int])

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c :: Integer -> Integer
c = fmap (* 2) (\x -> x - 2)

d :: Integer -> [Char]
d = fmap ((return '1' ++) . show) (\x -> [x, 1 .. 3])

e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = fmap read (fmap ("123" ++) (fmap show ioi))
   in fmap (* 3) changed

