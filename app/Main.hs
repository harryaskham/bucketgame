{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Concurrent.MVar
import System.Random
import System.Random.Shuffle
import Safe
import Web.Firefly
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.Status
import qualified Data.Map.Strict as M
import Control.Monad.IO.Class
import Data.Char
import Control.Lens
import Text.Blaze.Html
import qualified Data.ByteString as B

type Entry = T.Text

data GameState = GameState { _entries :: MVar [Entry]
                           , _unseen :: MVar [Entry]
                           }
makeLenses ''GameState

cors :: ToResponse b => b -> (b, Status, HeaderMap)
cors b = (b, status200, M.fromList [("Access-Control-Allow-Origin", ["*"])])

addHandler :: GameState -> Handler (T.Text, Status, HeaderMap)
addHandler st = do
  name <- getQuery "name"
  case name of
       Just n ->
         if all isSpace (T.unpack n)
            then do
              liftIO $ print "empty thing"
              return $ cors "fail"
            else do
              liftIO $ print "Adding an entry"
              liftIO $ modifyMVar_ (st^.entries) (pure . (n:))
              return $ cors "added"
       Nothing -> do
         liftIO $ print "No name"
         return $ cors "failure"

resetHandler :: GameState -> Handler (T.Text, Status, HeaderMap)
resetHandler st = do
  g <- liftIO newStdGen
  liftIO $ print "resetting the round"
  es <- liftIO $ readMVar (st^.entries)
  liftIO $ modifyMVar_ (st^.unseen) (const . return $ shuffle' es (length es) g)
  return $ cors "reset"

takeHandler :: GameState -> Handler (T.Text, Status, HeaderMap)
takeHandler st = do
  es <- liftIO $ readMVar (st^.unseen)
  case headMay es of
    Just e -> do
      liftIO $ print "taking one thing"
      liftIO $ modifyMVar_ (st^.unseen) (const . return $ tailSafe es)
      return $ cors e
    Nothing -> do
      liftIO $ print "nothing left"
      return $ cors "empty"

putBackHandler :: GameState -> Handler (T.Text, Status, HeaderMap)
putBackHandler st = do
  name <- getQuery "name"
  case name of
    Just n -> do
      g <- liftIO newStdGen
      liftIO $ print "putting back"
      us <- liftIO $ readMVar (st^.unseen)
      let newUs = n:us
      liftIO $ modifyMVar_ (st^.unseen) (const . return $ shuffle' newUs (length newUs) g)
      return $ cors "put back"
    Nothing -> do
      liftIO $ print "invalid putback"
      return $ cors "invalid"

main :: IO ()
main = do
  eM <- newMVar []
  uM <- newMVar []
  let state = GameState { _entries = eM
                        , _unseen = uM
                        }
  run 3000 $ do
    route "/add" $ addHandler state
    route "/reset" $ resetHandler state
    route "/take" $ takeHandler state
    route "/putback" $ putBackHandler state
