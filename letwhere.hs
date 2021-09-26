module Mult1 where

mult1 = x * y
  where
    x = 5
    y = 6

mult1a = 
  let x = 5; y = 6
  in x * y

expr2 = x * 3 + y
  where
    x = 3
    y = 1000

expr2a =
  let 
    x = 3
    y = 1000
  in
    x * 3 + y

expr3 = x * 5
  where
    x = 10 * 5 + y
    y = 10

expr3a = 
  let 
    x = 10 * 5 + y
    y = 10
  in
    x * 5


main = do
  print mult1
  print mult1a
  print expr2
  print expr2a
  print expr3
  print expr3a

