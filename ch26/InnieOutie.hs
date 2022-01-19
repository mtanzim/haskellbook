module InnieOutie where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

main :: IO (Either String (Maybe Int))
main = readerUnwrap ()

-- instance Monad ((->) r) where return = const
-- instance Monad (Either e) where return = Right
-- instance Monad Maybe where return = Just

main' :: Either String (Maybe Integer)
main' = ((const . Right . Just) 1) ()

rembed :: MaybeT (ExceptT String (ReaderT () IO)) Int
-- wrong
-- rembed = ReaderT $ \r -> (ExceptT <$> (MaybeT <$> const (Right (Just 1))))
rembed = MaybeT . ExceptT . ReaderT $ const (return (Right (Just 1)))