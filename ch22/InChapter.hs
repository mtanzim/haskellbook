{-# LANGUAGE InstanceSigs #-}

module InChapter where

newtype Reader r a = Reader {runReader :: r -> a}

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ \r -> f (ra r)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader (\_ -> a)
  (<*>) ::
    Reader r (a -> b) ->
    Reader r a ->
    Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)
