module ChapterExercises where

import Control.Applicative (liftA3)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  Pair f f' <*> Pair x x' = Pair (f x) (f' x')

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
  a <- arbitrary
  return (Pair a a)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = pairGen

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (Two a fb) <*> (Two a' b) = Two (a <> a') (fb b)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  Three a b fc <*> Three a' b' c' = Three (a <> a') (b <> b') (fc c')

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

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

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  Three' a fb fb' <*> Three' a' b b' = Three' (a <> a') (fb b) (fb' b')

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

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  Four a b c fd <*> Four a' b' c' d = Four (a <> a') (b <> b') (c <> c') (fd d)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  Four' a1 a2 a3 fb <*> Four' a1' a2' a3' b = Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (fb b)

fourGen' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
fourGen' = do
  a <- arbitrary
  b <- arbitrary
  return (Four' a a a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = fourGen'

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

-- See this: https://github.com/mtanzim/haskellbook/blob/98a2ca3ab061bb77fb90e7e7dd22e39dfa7d396c/ch17/exercises.hs#L81
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
-- combos a b c = [(s, v, s') | s <- a, v <- b, s' <- c]
-- combos a b c = (((fmap (,,) a) <*> b) <*> c)
-- combos a b c = (,,) <$> a <*> b <*> c
-- combos a b c = liftA3 (,,) a b c
combos = liftA3 (,,)

main :: IO ()
main = do
  quickBatch $ applicative (Pair ('a', 'b', 'c') ('d', 'e', 'f'))
  quickBatch $ applicative (Two ("abc", "bbc", "cbc") ("abc", "bbc", "cbc"))
  quickBatch $ applicative (Three ("abc", "bbc", "cbc") ("abc", "bbc", "cbc") ("abc", "bbc", "cbc"))
  quickBatch $ applicative (Three' ("abc", "bbc", "cbc") ("abc", "bbc", "cbc") ("abc", "bbc", "cbc"))
  quickBatch $ applicative (Four ("abc", "bbc", "cbc") ("abc", "bbc", "cbc") ("abc", "bbc", "cbc") ("abc", "bbc", "cbc"))
  quickBatch $ applicative (Four' ("abc", "bbc", "cbc") ("abc", "bbc", "cbc") ("abc", "bbc", "cbc") ("abc", "bbc", "cbc"))
  print (combos stops vowels stops)