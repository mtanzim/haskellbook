module Arith4 where

roundTrip :: (Show a, Read a, Read b) => a -> b
roundTrip = read . show

main :: IO ()
main = do
  print (roundTrip 4 :: Int)
  print (id 4)