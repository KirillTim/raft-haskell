{-# LANGUAGE OverloadedStrings #-}
module NetworkUtils
  ( rightOrThrow
  , readConfig
  , restartTimer
  , stopTimer
  ) where

import           Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import           Control.Exception  (Exception, throw)
import           Data.IORef         (IORef, readIORef, writeIORef)
import           System.IO          (readFile)

import qualified Data.Text          as T (pack, splitOn, unpack)

import           Types

stopTimer :: IORef (Maybe ThreadId) -> IO ()
stopTimer ref = do
  mtid <- readIORef ref
  case mtid of Just tid -> killThread tid
               Nothing  -> return ()
  --fmap killThread tid  WTF?!!

restartTimer :: Int -> IORef (Maybe ThreadId) -> IO () -> IO ThreadId
restartTimer delay ref action = do
  stopTimer ref
  new <- startTimer delay action
  writeIORef ref $ Just new
  return new

startTimer :: Int -> IO () -> IO ThreadId
startTimer delay action = forkIO $ do
  threadDelay delay
  action

rightOrThrow :: Exception e => Either e b -> IO b
rightOrThrow (Left err) = throw err
rightOrThrow (Right x)  = return x

readConfig :: String -> String -> IO Config
readConfig file nodeName = do
  content <- readFile file
  let info = map parseNodeInfo $ lines content
  let self = head $ filter (\n -> _name n == nodeName) info
  let others = filter (\n -> _name n /= nodeName) info
  return $ Config self others

parseNodeInfo :: String -> NodeInfo
parseNodeInfo str = NodeInfo name addr port True
               where [name, adp] = words str
                     [addr, port] = map T.unpack $ T.splitOn ":" $ T.pack adp
