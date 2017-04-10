{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent         (ThreadId, forkIO)
import           Control.Concurrent.STM     (STM, TChan, TVar, atomically,
                                             modifyTVar, newTChan, newTVarIO,
                                             readTChan, readTVar, readTVarIO,
                                             writeTChan)
import           Network.Transport
import           Network.Transport.TCP      (createTransport,
                                             defaultTCPParameters)

import           Control.Lens               ((^.))

import           Control.Monad              (forM_, forever)
import           Control.Monad.RWS.Strict   (execRWST)

import           Data.Aeson                 (encode)
import           Data.ByteString.Char8      as BS (pack)
import           Data.ByteString.Lazy.Char8 as LBS (unpack)
import           Data.IORef                 (newIORef)
import qualified Data.Map                   as M (Map, empty, insert, keys,
                                                  lookup)
import           System.Environment         (getArgs)

import           Handlers
import           NetworkUtils
import           Types                      (Config (..), MessageFrom (..),
                                             MessageTo (..), NodeInfo (..),
                                             NodeState (..), addr, host,
                                             initNodeState, others, port, self)

type NodeConnsTVar = TVar (M.Map String Connection)
type ClientConnsTVar = TVar (M.Map ConnectionId Connection)

oneMilliSecond :: Int
oneMilliSecond = 1000 :: Int
oneSecond :: Int
oneSecond = 1000 * oneMilliSecond

main :: IO ()
main = do
  args <- getArgs
  let name = if null args
        then error "first argument should be name of node to launch"
        else head args
  cfg <- readConfig "nodes.cfg" name
  transport <- rightOrThrow =<< createTransport (cfg^.self.host) (cfg^.self.port) defaultTCPParameters
  selfEndpoint <- rightOrThrow =<< newEndPoint transport
  clientConns <- newTVarIO (M.empty :: M.Map ConnectionId Connection)
  nodeConns <- newTVarIO (M.empty ::  M.Map String Connection)
  forM_ (cfg^.others) $ \n -> do
    let eaddr = EndPointAddress $ pack $ n^.host ++ ":" ++ n^.port ++ ":0"
    ce <- connect selfEndpoint eaddr ReliableOrdered defaultConnectHints
    case ce of Right conn -> do
                   putStrLn $ "connected to " ++ _name n ++ " " ++ show eaddr
                   atomically $ modifyTVar nodeConns $ M.insert (_name n) conn
               Left _ -> putStrLn $ "can't connect to " ++ _name n

  inputChan <- atomically (newTChan :: STM (TChan MessageFrom))
  outputChan <- atomically (newTChan :: STM (TChan MessageTo))

  _ <- forkIO $ inServer selfEndpoint nodeConns clientConns
  _ <- forkIO $ outServer outputChan inputChan nodeConns clientConns
  _ <- forkIO $ logicThread cfg inputChan outputChan

  c <- readTVarIO nodeConns
  print $ M.keys c

  _ <- getLine
  --readmit serverDone `onCtrlC` do
  closeTransport transport
  closeEndPoint selfEndpoint

logicThread :: Config -> TChan MessageFrom -> TChan MessageTo -> IO () -- TODO: make interface ?
logicThread config inChannel outChannel = do
  atomically $ writeTChan outChannel StartElectionTimeout --  FIXME
  go config initNodeState
  where
    go :: Config -> NodeState -> IO ()
    go cfg nodeState = do
      msg <- atomically $ readTChan inChannel
      case msg of (MessageFromNode fromN _) -> do
                    putStrLn $ "msg from " ++ fromN
                    go cfg nodeState
                  ElectionTimeout -> do
                    (s, outbox) <- execRWST handleElectionTimeout cfg nodeState
                    forM_ outbox $ \x -> atomically $ writeTChan outChannel x
                    go cfg s
                  _ -> go cfg nodeState
      go cfg nodeState

outServer :: TChan MessageTo -> TChan MessageFrom -> NodeConnsTVar -> ClientConnsTVar -> IO () -- TODO: better naming ?
outServer outChannel inChannel nodesTV clientsTV = go
  where
    go :: IO ()
    go = do
      electionTimer <- newIORef (Nothing :: Maybe ThreadId) -- TODO: custom type
      heartbeatTimer <- newIORef (Nothing :: Maybe ThreadId)
      forever $ do
        msg <- atomically $ readTChan outChannel
        putStrLn $ "outServer: " ++ show msg
        case msg of (MessageToNode toN m) -> do
                      nodesC <- atomically $ readTVar nodesTV
                      let x = M.lookup toN nodesC
                      case x of Just c -> do -- TODO: >>=
                                  _ <- send c [BS.pack $ LBS.unpack $ encode m] -- rewrite
                                  return ()
                                Nothing -> putStrLn $ "no connection to " ++ show toN
                    (ClientCommandResponse ccmd _) -> do
                      _ <- atomically $ readTVar clientsTV
                      let _ =  ccmd^.addr
                      return ()
                    StartElectionTimeout -> do
                      putStrLn "start election timer"
                      _ <- restartTimer (3 * oneSecond) electionTimer $ sendElectionTimerMsg inChannel
                      return ()
                    StopElectionTimeout  -> do
                      putStrLn "stop election timer"
                      stopTimer electionTimer
                    StartHeartbeatTimout -> do
                      putStrLn "start heartbeat timer"
                      _ <- restartTimer (5 * oneSecond) heartbeatTimer $ sendHeartBeatTimerMsg inChannel
                      return ()
                    StopHeartbeatTimeout -> do
                      putStrLn "stop heartbeat timer"
                      stopTimer heartbeatTimer

sendElectionTimerMsg :: TChan MessageFrom -> IO ()
sendElectionTimerMsg = sendTimerMsg ElectionTimeout
sendHeartBeatTimerMsg :: TChan MessageFrom -> IO ()
sendHeartBeatTimerMsg = sendTimerMsg HeartbeatTimeout

sendTimerMsg :: MessageFrom -> TChan MessageFrom -> IO ()
sendTimerMsg msg chan = atomically $ writeTChan chan msg

inServer :: EndPoint -> NodeConnsTVar -> ClientConnsTVar -> IO ()
inServer endpoint _ _ = go
  where
    go :: IO ()
    go = do
      event <- receive endpoint
      case event of
        ConnectionOpened _ _ caddr -> do
          putStrLn $ "connection is opened " ++ show caddr
          go
        Received _ payload -> do
          putStrLn $ "received " ++ show payload
          go
        ConnectionClosed _ -> go
        EndPointClosed -> putStrLn "Echo server exiting"
        _ -> go
