module ChapterExercises where

import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck (CoArbitrary)
import Test.QuickCheck.Arbitrary (Arbitrary)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

type TwoAssoc = Two String [Int] -> Two String [Int] -> Two String [Int] -> Bool

-- TODO: `Three` and `Four` would be the same?

newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

boolConjGen :: Gen BoolConj
boolConjGen = elements [BoolConj True, BoolConj False]

instance Arbitrary BoolConj where
  arbitrary = boolConjGen

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Semigroup BoolDisj where
  BoolDisj True <> _ = BoolDisj True
  _ <> BoolDisj True = BoolDisj True
  _ <> _ = BoolDisj False

boolDisjGen :: Gen BoolDisj
boolDisjGen = elements [BoolDisj True, BoolDisj False]

instance Arbitrary BoolDisj where
  arbitrary = boolDisjGen

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) (Snd b) _ = Snd b
  (<>) (Fst _) (Snd b) = Snd b
  (<>) (Fst a) (Fst a') = Fst a'

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
  a <- arbitrary
  b <- arbitrary
  elements [Fst a, Snd b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGen

type OrAssoc = (Or Int String) -> (Or Int String) -> (Or Int String) -> Bool

newtype Combine a b = Combine {unCombine :: a -> b}

instance Show (Combine a b) where
  show _ = "hello"

instance Eq (Combine a b) where
  (==) _ _ = True

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

-- TODO: study CoArbitrary
combineGen :: (CoArbitrary f, Arbitrary b) => Gen (Combine f b)
combineGen = do
  f <- arbitrary
  return (Combine f)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = combineGen

type CombineAssoc = Combine (String -> [Int]) String -> Combine (String -> [Int]) String -> Combine (String -> [Int]) String -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: CombineAssoc)
