module Ch10 where

fibs = 1 : scanl (+) 1 fibs

fibsN x = fibs !! x

fibsCapped = (flip take) fibs
