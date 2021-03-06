{-# LANGUAGE InstanceSigs #-}

module Compose where

newtype Compose f g a = Compose {getCompose :: f (g a)}
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- ¯\_(ツ)_/¯
-- TODO: come back to this, still does't quite make sense
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose (pure (pure x))
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  -- see pg 991
  Compose f <*> Compose x = Compose $ (<*>) <$> f <*> x

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga