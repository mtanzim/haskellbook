module InChapter where

twiceWhenEven :: [Integer] -> [Integer] -> [Integer]
twiceWhenEven xs ys = do
  x <- xs
  if even x
    then do
      y <- ys
      if even y
        then [x, y]
        else [y]
    else [x]

crossProduct :: [Integer] -> [Integer] -> [Integer]
crossProduct xs ys = do
  x <- xs
  y <- ys
  [x, y]
