module Exercises where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = ReaderT $ \r -> return (r - 1)

rDec' :: Num a => Reader a a
rDec' = ReaderT $ return . (+ (-1))

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \a -> return (show a)

rShow' :: Show a => ReaderT a Identity String
rShow' = ReaderT $ return . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
  putStrLn ("Hi: " ++ show a)
  return (a + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \a -> do
  putStrLn ("Hi: " ++ show a)
  return (show a, a + 1)
