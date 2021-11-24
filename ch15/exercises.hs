module Exercises where

import Test.QuickCheck

data Booly a = False' | True' deriving (Eq, Show)

instance Semigroup (Booly a) where
  (<>) False' _ = False'
  (<>) _ False' = False'
  (<>) True' True' = True'

instance Monoid (Booly a) where
  mempty = True'

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Semigroup (Optional a) where
  (<>) (Only a) (Only a') = Only (a `mappend` a')
  (<>) (Only a) Nada = Only a
  (<>) Nada (Only a) = Only a
  (<>) _ _ = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbinBetter :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter e adv noun adj =
  mconcat
    [ e,
      "! he said ",
      adv,
      " as he jumped into his car ",
      noun,
      " and drove off with his ",
      adj,
      " wife"
    ]

newtype First' a = First' {getFirst :: Optional a}
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) = undefined

instance Monoid (First' a) where
  mempty = undefined

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
