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

bumpBoomp :: String -> M.Map String Integer -> (M.Map String Integer, Integer)
bumpBoomp k m =
  let newVal = (+) 1 $ M.findWithDefault 0 k m
      updatedM = M.insert k newVal m
   in (updatedM, newVal)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    prefix <- lift $ asks prefix
    curMRef <- lift $ asks counts
    curM <- (lift . lift) $ readIORef curMRef
    let key' = mappend prefix unprefixed
        (updatedM, newVal) = bumpBoomp key' curM
    lift . lift $ writeIORef curMRef updatedM
    html $ mconcat ["<h1>Success! Count was: ", TL.pack $ show newVal, "</h1>"]

main :: IO ()
main = do
  -- [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter "lol"
      -- TODO: don't understand this :(
      runR r = runReaderT r config
  scottyT 3000 runR app