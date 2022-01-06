{-# LANGUAGE InstanceSigs #-}
module InChapter where

newtype Reader r a = Reader {runReader :: r -> a}

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

