module Exercises where

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
