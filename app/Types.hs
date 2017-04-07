{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import qualified Data.Map as M

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Control.Monad.RWS.Strict --(RWS, MonadReader, MonadWriter, MonadState,
                                -- ask, tell, get, put, execRWS, liftIO)

import Control.Lens (makeLenses)

data Role = Candidate | Follower | Leader deriving (Show, Eq)

newtype Term = Term Int deriving (Show, Eq, Ord, Num, Generic, Typeable)
newtype LogIndex = LogIndex Int deriving (Show, Eq, Ord, Generic, Typeable)

initTerm = Term 0
initIndex = LogIndex (-1)


data LogEntry = LogEntry
  { _index :: LogIndex,
    _term  :: Term,
    _key   :: String,
    _value :: String
  } deriving (Show, Generic, Typeable)

data NodeInfo = NodeInfo
  { _name    :: String,
    _address :: String,
    _port    :: Int,
    _isAlive :: Bool
  } deriving (Show, Eq)

data Config = Config
  { _self   :: NodeInfo,
    _others :: [NodeInfo]
  } deriving (Show, Eq)

data NodeName = String deriving (Show, Generic, Typeable)

data NodeState = NodeState
  { _role               :: Role,
    _config             :: Config,
    _currentTerm        :: Term,
    _votedForOnThisTerm :: Maybe NodeInfo,
    _log                :: [LogEntry],
    _commitIndex        :: LogIndex,
    _lastApplied        :: LogIndex,
    _currentLeader      :: Maybe NodeInfo,
    _votesForMe         :: Int,
    _nextIndex          :: M.Map NodeName LogIndex,
    _matchIndex         :: M.Map NodeName LogIndex,
    _storage            :: M.Map String String
  } deriving (Show)

makeLenses ''NodeState

testNodes :: Int -> [NodeInfo]
testNodes n = fmap (\x -> NodeInfo ("node"++show x) "localhost" (9000+x) True) [1..n]

initTestConfig :: Config
initTestConfig = Config
  (head $ testNodes 3)
  (tail $ testNodes 3)

initNodeState :: NodeState
initNodeState = NodeState
  Follower
  initTestConfig
  initTerm
  Nothing
  []
  initIndex
  initIndex
  Nothing
  0
  M.empty
  M.empty
  M.empty

--messages
data Message =
  AppendEntries
  { _term    :: Term,
    _leaderName    :: String,
    _prevLogIndex  :: LogIndex,
    _prevLogTerm   :: Term,
    _entries       :: [LogEntry],
    _leaderCommit  :: LogIndex
  }
  | AppendRejected
  { _nodeName :: String,
    _term     :: Term
  }
  | AppendSuccessfull
  { _nodeName  :: String,
    _term      :: Term,
    _lastIndex :: LogIndex
  }
  | RequestVote
  { _term :: Term,
    _candidateName :: String,
    _lastLogIndex  :: LogIndex,
    _lastLogTerm   :: Term
  }
  | VoteForCandidate { _term :: Term }
  | DeclineCandidate { _term :: Term }
  | ElectionTimeout
  | HeartbeatTimeout deriving (Show, Generic, Typeable)

data AddressedMessage = AddressedMessage
  { recipient :: NodeName,
    message   :: Message
  } deriving (Show, Generic, Typeable)

newtype NodeAction a = NodeAction {runAction :: RWST Config [AddressedMessage] NodeState IO a}
    deriving (Functor, Applicative, Monad, MonadState NodeState,
              MonadWriter [AddressedMessage], MonadReader Config)

data LittleCfg = LittleCfg { _ignore :: [Int]} deriving (Show)
data LittleState = LittleState { _next :: Int } deriving (Show)

comp :: Int -> RWST LittleCfg [String] LittleState IO ()
comp start = do
  r <- ask
  let add = case start `elem` _ignore r of True -> 1
                                           False -> start
  tell ["start is " ++ show start]
  tell ["add is " ++ show add]
  liftIO $ putStrLn $ show add
  prev <- get
  let new = prev { _next = _next prev + add}
  put new
  tell ["new is " ++ show new]
  
{-comp :: Int -> RWST [Int] [Int] Int IO ()
comp start = do
  r <- ask
  let add = case start `elem` r of True  -> 1
                                   False -> start
  liftIO $ putStrLn $ show add
  prev <- get
  let new = prev + add
  put new
  tell [start]
  if new < 100 then runRWST (comp new) r prev else return () -}
