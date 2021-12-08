module FunctorInstances where

import Test.QuickCheck
import Test.QuickCheck (Arbitrary)

-- Instances of Functor

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = pairGen

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
  a <- arbitrary
  return (Pair a a)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  fmap Identity arbitrary

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

threeGen' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
threeGen' = do
  a <- arbitrary
  b <- arbitrary
  return (Three' a b b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = threeGen'

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

fourGen' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
fourGen' = do
  a <- arbitrary
  b <- arbitrary
  return (Four' a a a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = fourGen'

-- NO, you cannot make a Functor instance of Trivial, since it has kind *

main :: IO ()
main = do
  quickCheck f
  quickCheck li
  quickCheck fPair
  quickCheck liPair
  quickCheck fTwo
  quickCheck liTwo
  quickCheck fThree
  quickCheck liThree
  quickCheck fThree'
  quickCheck liThree'
  quickCheck fFour
  quickCheck liFour
  quickCheck fFour'
  quickCheck liFour'
  where
    f :: Identity Int -> Bool
    f x = functorIdentity x
    fPair :: Pair Int -> Bool
    fPair x = functorIdentity x
    fTwo :: Two Int Char -> Bool
    fTwo x = functorIdentity x
    fThree :: Three Int Char String -> Bool
    fThree x = functorIdentity x
    fThree' :: Three' Int Char -> Bool
    fThree' x = functorIdentity x
    fFour :: Four Int Char Char Double -> Bool
    fFour x = functorIdentity x
    fFour' :: Four' Int Char -> Bool
    fFour' x = functorIdentity x
    c = functorCompose (+ 1) (* 2)
    li x = c (x :: Identity Int)
    cPair = functorCompose (+ 1) (* 2)
    liPair x = cPair (x :: Pair Int)
    cTwo = functorCompose (const Two 44 "s") (const "s")
    liTwo x = cTwo (x :: Two Int String)
    cThree = functorCompose (const Three 44 "s" 'g') (const "s")
    liThree x = cThree (x :: Three Int String Int)
    cThree' = functorCompose (const Three' 44 "s" "g") (const "s")
    liThree' x = cThree' (x :: Three' Int String)
    cFour = functorCompose (* 3) (+ 1)
    liFour x = cFour (x :: Four String Char Double Int)
    cFour' = functorCompose (* 3) (+ 1)
    liFour' x = cFour' (x :: Four' String Int)
