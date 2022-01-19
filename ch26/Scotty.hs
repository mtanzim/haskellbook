{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Control.Monad.Trans.Class
import Data.Monoid (mconcat)
import Web.Scotty

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    let hello = putStrLn "hello"
    (lift :: IO a -> ActionM a) hello
    html $ mconcat ["<h1>Scotty, ", beam, " me up! </h1>"]