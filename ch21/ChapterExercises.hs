module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant a) <*> (Constant a') = Constant (a <> a')

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure (Constant a)

constantGen :: (Arbitrary a, Arbitrary b) => Gen (Constant a b)
constantGen = do
  a <- arbitrary
  return (Constant a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = constantGen

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

data BahEither b a = PRight b | PLeft a deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a) = PLeft (f a)

instance Monoid b => Applicative (BahEither b) where
  pure a = PLeft a
  PRight b <*> PRight b' = PRight (b <> b')
  PRight b <*> _ = PRight b
  _ <*> PRight b = PRight b
  PLeft f <*> PLeft a = PLeft (f a)

instance Monoid b => Monad (BahEither b) where
  return = pure
  (PRight b) >>= _ = PRight b
  (PLeft a) >>= f = f a

instance Monoid b => Foldable (BahEither b) where
  foldMap f (PRight b) = mempty
  foldMap f (PLeft a) = f a

instance Monoid b => Traversable (BahEither b) where
  traverse f (PRight b) = pure (PRight b)
  traverse f (PLeft a) = PLeft <$> f a

genEither :: (Arbitrary a, Arbitrary b) => Gen (BahEither b a)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [PLeft a, PRight b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = genEither

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

-- List

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a lst) = Cons (f a) (fmap f lst)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons a Nil) >>= f = f a
  (Cons a as) >>= f = append (f a) (as >>= f)

instance Foldable List where
  foldMap f Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

listGen :: Arbitrary a => Gen (List a)
listGen = do
  a <- arbitrary
  lst <- listGen
  frequency [(1, return Nil), (1, return (Cons a lst))]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen

instance Eq a => EqProp (List a) where
  (=-=) = eq

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  Three a b fc <*> Three a' b' c' = Three (a <> a') (b <> b') (fc c')

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

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

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (Two a fb) <*> (Two a' b) = Two (a <> a') (fb b)

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

instance Traversable (Two a) where
  traverse f (Two a b) = Two a <$> f b

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- Big
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

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b <> f b'

instance Traversable (Three' a) where
  traverse f (Three' a b b') = Three' a <$> f b <*> f b'

-- Bigger
data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

biggerGen :: (Arbitrary a, Arbitrary b) => Gen (Bigger a b)
biggerGen = do
  a <- arbitrary
  b <- arbitrary
  return (Bigger a b b b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = biggerGen

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance Monoid a => Applicative (Bigger a) where
  pure b = Bigger mempty b b b
  Bigger a fb fb' fb'' <*> Bigger a' b b' b'' = Bigger (a <> a') (fb b) (fb' b') (fb'' b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger a b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

test trigger = do
  quickBatch $ traversable trigger

type Trigger = ([Int], [Int], [Char])

main :: IO ()
main = do
  let trigger = undefined
  test (trigger :: Identity Trigger)
  test (trigger :: Constant Int Trigger)
  test (trigger :: BahEither Trigger Trigger)
  test (trigger :: List Trigger)
  test (trigger :: Three Trigger Trigger Trigger)
  test (trigger :: Three' Trigger Trigger)
  test (trigger :: Bigger Trigger Trigger)