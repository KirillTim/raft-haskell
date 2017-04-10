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

import           Data.ByteString.Char8        as BS (pack)
import qualified Data.Map                     as M
import qualified Data.Text                    as T

import           System.Environment
import           System.IO

import           Types

run :: String -> IO ()
run port = do
  let host = "localhost"
  putStrLn "run"

main :: IO ()
main = do
  --let [name] = ["node1"]
  [name] <- getArgs
  cfg <- readConfig "nodes.cfg" name
  transport <- rightOrThrow =<< createTransport (cfg^.self.host) (cfg^.self.port) defaultTCPParameters
  selfEndpoint <- rightOrThrow =<< newEndPoint transport
  let connections = newTVar M.empty
  nodeConns <- newTVarIO (M.empty :: M.Map String Connection)
  forM_ (cfg^.others) $ \n -> do
    let addr = EndPointAddress $ pack $ n^.host ++ ":" ++ n^.port ++ ":0"
    ce <- connect selfEndpoint addr ReliableOrdered defaultConnectHints
    case ce of Right conn -> do
                   putStrLn $ "connected to " ++ (_name n) ++ " " ++ (show addr)
                   atomically $ modifyTVar nodeConns $ M.insert (_name n) conn
               Left err -> putStrLn $ "can't connect to " ++ (_name n)

  serverDone <- newEmptyMVar
  forkIO $ inServer selfEndpoint nodeConns serverDone

  c <- readTVarIO nodeConns
  putStrLn $ show $ M.keys c

  readMVar serverDone `onCtrlC` do
    closeTransport transport
    closeEndPoint selfEndpoint

inServer :: EndPoint -> TVar (M.Map String Connection) -> MVar () -> IO ()
inServer endpoint conns  serverDone = go
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
