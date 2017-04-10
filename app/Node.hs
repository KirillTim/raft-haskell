{-# LANGUAGE OverloadedStrings #-}
module Node where
import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Exception

import Control.Lens

import qualified Data.Text as T

import System.IO
import System.Environment

import Types

run :: String -> IO()
run port = do
  let host = "localhost"
  putStrLn "run"

main :: IO ()
main = do
  [name] <- getArgs
  cfg <- readConfig "nodes.cfg" name
  transportE <- createTransport (cfg^.self.host) (cfg^.self.port) defaultTCPParameters
  let transport = case transportE of (Left err) -> throw err
                                     (Right x) -> x
  Right selfEndpoint <- newEndPoint transport
  return ()


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
