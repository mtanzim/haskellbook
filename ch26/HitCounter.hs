{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config = Config
  { counts :: IORef (M.Map String Integer),
    prefix :: String
  }

type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = undefined

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    prefix <- lift $ asks prefix
    curMRef <- lift $ asks counts
    curM <- (lift . lift) $ readIORef curMRef
    let key' = mappend prefix unprefixed
        newVal = (+) 1 $ M.findWithDefault 0 key' curM
        updatedM = M.insert key' newVal curM
    lift . lift $ writeIORef curMRef updatedM
    html $ mconcat ["<h1>Success! Count was: ", TL.pack $ show newVal, "</h1>"]

main :: IO ()
main = do
  -- [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter "lol"
      runR r = runReaderT r config
  scottyT 3000 runR app