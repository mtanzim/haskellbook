{-# LANGUAGE InstanceSigs #-}

module MaybeT where

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT fma) = MaybeT $ (fmap . fmap) f fma

instance (Applicative m) => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure x = MaybeT (pure (pure x))
  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  MaybeT f <*> MaybeT x = MaybeT $ (<*>) <$> f <*> x

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT ma >>= f = MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeT $ f y
-- See pg 993
-- y :: a
-- f :: a -> MaybeT m b
-- f y :: MaybeT m b
-- runMaybeT (f y) :: m (Maybe b)