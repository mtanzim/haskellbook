{-# LANGUAGE InstanceSigs #-}

module BruhYouLift where

import Control.Monad (liftM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import EitherT

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift ma = (EitherT . liftM Right) ma

-- newtype StateT' s m a = StateT' {runStateT :: s -> m (a, s)}

-- newtype State' s a = State' {runState :: s -> (a, s)}

-- not sure if this works
instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift ma = StateT $ liftM State (\s -> (ma, s))