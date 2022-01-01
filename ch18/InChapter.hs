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

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Monoid a => Applicative (Sum a) where
  pure b = Second b
  First a <*> First a' = First (a <> a')
  First a <*> _ = First a
  _ <*> First a = First a
  Second f <*> Second a = Second (f a)

instance Monoid a => Monad (Sum a) where
  return = pure
  (First a) >>= _ = First a
  (Second b) >>= f = f b
