{-# LANGUAGE InstanceSigs #-}

module IdentityT where

import Control.Monad (join)

newtype IdentityT f a = IdentityT {runIdentityT :: f a} deriving (Eq, Show)

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance Applicative m => Applicative (IdentityT m) where
  pure x = IdentityT $ pure x
  (IdentityT fab) <*> (IdentityT fa) = IdentityT $ fab <*> fa

instance Monad m => Monad (IdentityT m) where
  return = pure
  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f =
    let -- Step 1: understand that we have `m (IdentityT m b)`
        -- aimb = fmap f ma
        -- remember m is a functor since it's a monad
        -- Step 2: now we get `m (m b)` by fmapping runIdentityT into `m (IdentityT m b)`
        -- aimb = fmap runIdentityT (fmap f ma)
        -- Step 3: Now, to to get `m b` we need join
        -- aimb = join $ fmap runIdentityT (fmap f ma)
        -- Step 4: Now, wrap it back into IdentityT
        -- aimb = IdentityT $ join (fmap runIdentityT (fmap f ma))
        -- Step 5: this is equivalent to (since `fmap(f.g) == fmap f. fmap g`)
        -- aimb = IdentityT $ join (fmap (runIdentityT . f) ma)
        -- Step 6: and since >>= is just fmap composed with join, we finally have
        aimb = IdentityT $ ma >>= runIdentityT . f
     in aimb