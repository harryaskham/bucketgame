{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar
import System.Random
import System.Random.Shuffle
import Safe
import Web.Firefly
import qualified Data.Text as T
import Network.HTTP.Types.Status
import qualified Data.Map.Strict as M
import Control.Monad.IO.Class
import Data.Char

type Entry = String

cors :: ToResponse b => b -> (b, Status, HeaderMap)
cors b = (b, status200, M.fromList [("Access-Control-Allow-Origin", ["*"])])

main :: IO ()
main = do
  entries <- newMVar []
  unseen <- newMVar []
  let addHandler :: Handler (T.Text, Status, HeaderMap)
      addHandler = do
        name <- getQuery "name"
        case name of
             Just n ->
               if all isSpace (T.unpack n)
                  then do
                    liftIO $ print "empty thing"
                    return $ cors "fail"
                  else do
                    liftIO $ print "Adding an entry"
                    liftIO $ modifyMVar_ entries (pure . (n:))
                    return $ cors "added"
             Nothing -> do
               liftIO $ print "No name"
               return $ cors "failure"
      resetHandler :: Handler (T.Text, Status, HeaderMap)
      resetHandler = do
        g <- liftIO newStdGen
        liftIO $ print "resetting the round"
        es <- liftIO $ readMVar entries
        liftIO $ modifyMVar_ unseen (const . return $ shuffle' es (length es) g)
        return $ cors "reset"
      takeHandler :: Handler (T.Text, Status, HeaderMap)
      takeHandler = do
        es <- liftIO $ readMVar unseen
        case headMay es of
          Just e -> do
            liftIO $ print "taking one thing"
            liftIO $ modifyMVar_ unseen (const . return $ tailSafe es)
            return $ cors e
          Nothing -> do
            liftIO $ print "nothing left"
            return $ cors "empty"
      putBackHandler :: Handler (T.Text, Status, HeaderMap)
      putBackHandler = do
        name <- getQuery "name"
        case name of
          Just n -> do
            g <- liftIO newStdGen
            liftIO $ print "putting back"
            us <- liftIO $ readMVar unseen
            let newUs = n:us
            liftIO $ modifyMVar_ unseen (const . return $ shuffle' newUs (length newUs) g)
            return $ cors "put back"
          Nothing -> do
            liftIO $ print "invalid putback"
            return $ cors "invalid"
  run 3000 $ do
    route "/add" addHandler
    route "/reset" resetHandler
    route "/take" takeHandler
    route "/putback" putBackHandler
