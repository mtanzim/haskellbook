module Exercises where

import Control.Monad.Trans.Reader

rDec :: Num a => Reader a a
rDec = ReaderT $ \r -> return (r - 1)

rDec' :: Num a => Reader a a
rDec' = ReaderT $ return . (+ (-1))
