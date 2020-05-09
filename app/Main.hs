{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar
import Web.Firefly
import Handlers

main :: IO ()
main = do
  eM <- newMVar []
  uM <- newMVar []
  let state = GameState { _entries = eM
                        , _unseen = uM
                        }
  run 3000 $ do
    route "/add" $ addThingHandler state
    route "/reset" $ resetHandler state
    route "/take" $ takeHandler state
    route "/putback" $ putBackHandler state
