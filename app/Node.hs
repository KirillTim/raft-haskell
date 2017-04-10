{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Exception
import           Control.Monad.STM
import           Network.Transport
import           Network.Transport.TCP        (createTransport,
                                               defaultTCPParameters)

import           Control.Lens

import           Control.Monad
import           Control.Monad.RWS.Strict

import           Data.Aeson
import           Data.ByteString.Char8        as BS (pack)
import           Data.ByteString.Lazy.Char8   as LBS (unpack)
import           Data.IORef
import qualified Data.Map                     as M
import qualified Data.Text                    as T
import           System.Environment
import           System.IO


import           Types
import Handlers

type NodeConnsTVar = TVar (M.Map String Connection)
type ClientConnsTVar = TVar (M.Map ConnectionId Connection)

oneMilliSecond = 1000 :: Int
oneSecond = 1000 * oneMilliSecond

main :: IO ()
main = do
  --let [name] = ["node1"]
  [name] <- getArgs
  cfg <- readConfig "nodes.cfg" name
  transport <- rightOrThrow =<< createTransport (cfg^.self.host) (cfg^.self.port) defaultTCPParameters
  selfEndpoint <- rightOrThrow =<< newEndPoint transport
  clientConns <- newTVarIO (M.empty :: M.Map ConnectionId Connection)
  nodeConns <- newTVarIO (M.empty ::  M.Map String Connection)
  forM_ (cfg^.others) $ \n -> do
    let addr = EndPointAddress $ pack $ n^.host ++ ":" ++ n^.port ++ ":0"
    ce <- connect selfEndpoint addr ReliableOrdered defaultConnectHints
    case ce of Right conn -> do
                   putStrLn $ "connected to " ++ (_name n) ++ " " ++ (show addr)
                   atomically $ modifyTVar nodeConns $ M.insert (_name n) conn
               Left err -> putStrLn $ "can't connect to " ++ (_name n)

  serverDone <- newEmptyMVar

  inputChan <- atomically (newTChan :: STM (TChan MessageFrom))
  outputChan <- atomically (newTChan :: STM (TChan MessageTo))

  forkIO $ inServer selfEndpoint nodeConns clientConns serverDone
  forkIO $ outServer outputChan inputChan nodeConns clientConns
  forkIO $ logicThread cfg inputChan outputChan

  c <- readTVarIO nodeConns
  putStrLn $ show $ M.keys c

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
      case msg of (MessageFromNode from message) -> do
                    putStrLn $ "msg from " ++ from
                    go cfg nodeState
                  ElectionTimeout -> do
                    (s, outbox) <- execRWST handleElectionTimeout cfg nodeState
                    forM_ outbox $ \x -> atomically $ writeTChan outChannel x
                    go cfg nodeState
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
        case msg of (MessageToNode to m) -> do
                      nodesC <- atomically $ readTVar nodesTV
                      let x = M.lookup to nodesC
                      case x of Just c -> do -- TODO: >>=
                                  send c [BS.pack $ LBS.unpack $ encode m] -- rewrite
                                  return ()
                                Nothing -> putStrLn $ "no connection to " ++ show to
                    (ClientCommandResponse cmd response) -> do
                      clientC <- atomically $ readTVar clientsTV
                      let cid =  cmd^.addr
                      return ()
                    StartElectionTimeout -> do
                      putStrLn "start election timer"
                      restartTimer (3 * oneSecond) electionTimer $ sendElectionTimerMsg inChannel
                      return ()
                    StopElectionTimeout  -> do
                      putStrLn "stop election timer"
                      stopTimer electionTimer
                    StartHeartbeatTimout -> do
                      putStrLn "start heartbeat timer"
                      restartTimer (5 * oneSecond) heartbeatTimer $ sendHeartBeatTimerMsg inChannel
                      return ()
                    StopHeartbeatTimeout -> do
                      putStrLn "stop heartbeat timer"
                      stopTimer heartbeatTimer

sendElectionTimerMsg = sendTimerMsg ElectionTimeout
sendHeartBeatTimerMsg = sendTimerMsg HeartbeatTimeout

sendTimerMsg :: MessageFrom -> TChan MessageFrom -> IO ()
sendTimerMsg msg chan = atomically $ writeTChan chan msg

stopTimer :: IORef (Maybe ThreadId) -> IO ()
stopTimer ref = do
  tid <- readIORef ref
  case tid of Just id -> killThread id
              Nothing -> return ()
  --fmap killThread tid  WTF?!!

restartTimer :: Int -> IORef (Maybe ThreadId) -> IO () -> IO ThreadId
restartTimer delay ref action = do
  stopTimer ref
  new <- startTimer delay action
  writeIORef ref $ Just new
  return new

startTimer :: Int -> IO () -> IO ThreadId
startTimer delay action = do
  forkIO $ do
    threadDelay delay
    action

inServer :: EndPoint -> NodeConnsTVar -> ClientConnsTVar -> MVar () -> IO ()
inServer endpoint nodes clients serverDone = go
  where
    go :: IO ()
    go = do
      event <- receive endpoint
      case event of
        ConnectionOpened cid rel addr -> do
          putStrLn $ "connection is opened " ++ show addr
          go
        Received cid payload -> do
          putStrLn $ "received " ++ show payload
          go
        ConnectionClosed cid -> do
          go
        EndPointClosed -> do
          putStrLn "Echo server exiting"
          putMVar serverDone ()

rightOrThrow :: Exception e => Either e b -> IO b
rightOrThrow (Left err) = throw err
rightOrThrow (Right x)  = return x

onCtrlC :: IO a -> IO () -> IO a
p `onCtrlC` q = catchJust isUserInterrupt p (const $ q >> p `onCtrlC` q)
  where
    isUserInterrupt :: AsyncException -> Maybe ()
    isUserInterrupt UserInterrupt = Just ()
    isUserInterrupt _             = Nothing

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
