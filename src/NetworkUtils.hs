{-# LANGUAGE OverloadedStrings #-}
module NetworkUtils
  ( rightOrThrow
  , readConfig
  , restartTimer
  , stopTimer
  ) where

import           Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import           Control.Exception  (Exception, throw)
import           Control.Lens       ((^.))
import           Data.IORef         (IORef, readIORef, writeIORef)
import           System.IO          (readFile)

import qualified Data.Text          as T (pack, splitOn, unpack)

import           Types              (Config(..), NodeInfo(..), name)

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
  let s = filter (\n -> n^.name == nodeName) info
  let self = if null s
             then error $ "can't find addr for " ++ nodeName ++ "in " ++ file
             else head s
  let others = filter (\n -> n^.name /= nodeName) info
  return $ Config self others

parseNodeInfo :: String -> NodeInfo
parseNodeInfo str = NodeInfo nname addr port True
               where [nname, adp] = words str
                     [addr, port] = map T.unpack $ T.splitOn ":" $ T.pack adp
