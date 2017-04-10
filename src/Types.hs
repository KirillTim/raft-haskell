{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types where

import           Control.Lens             (makeLenses)
import           Control.Monad.RWS.Strict (RWST (..))
import           Data.Typeable            (Typeable)
import           GHC.Generics             (Generic)

import qualified Data.Map                 as M (Map, empty)

import           Data.Aeson               (FromJSON, ToJSON)


data Role = Candidate | Follower | Leader deriving (Show, Eq)

newtype Term = Term Int deriving (Show, Eq, Ord, Num,  Generic, Typeable)
instance ToJSON Term
instance FromJSON Term

newtype LogIndex = LogIndex Int deriving (Show, Eq, Ord, Num, Generic, Typeable)
instance ToJSON LogIndex
instance FromJSON LogIndex

initTerm :: Term
initTerm = Term 0
initIndex :: LogIndex
initIndex = LogIndex (-1)

data Command =
  Remove { _key :: String }
  | Put { _key :: String, _value :: String}
  deriving (Show, Eq, Generic, Typeable)

instance ToJSON Command
instance FromJSON Command
makeLenses ''Command

data LogEntry = LogEntry
  { _index   :: LogIndex,
    _term    :: Term,
    _command :: Command
  } deriving (Show, Eq, Generic, Typeable)

instance ToJSON LogEntry
instance FromJSON LogEntry
makeLenses ''LogEntry

data NodeInfo = NodeInfo
  { _name    :: String,
    _host    :: String,
    _port    :: String,
    _isAlive :: Bool
  } deriving (Show, Eq)

makeLenses ''NodeInfo

data Config = Config
  { _self   :: NodeInfo,
    _others :: [NodeInfo]
  } deriving (Show, Eq)

makeLenses ''Config

data ClientCommand = ClientCommand
  { _addr :: String,
    _cmd  :: Command
  } deriving (Show, Eq, Generic, Typeable)

instance ToJSON ClientCommand
instance FromJSON ClientCommand
makeLenses ''ClientCommand

data NodeState = NodeState
  { _role                  :: Role,
    _currentTerm           :: Term,
    _votedForOnThisTerm    :: Maybe String,
    _eLog                  :: [LogEntry],
    _commitIndex           :: LogIndex,
    _lastApplied           :: LogIndex,
    _currentLeader         :: Maybe String,
    _votesForMe            :: Int,
    _nextIndex             :: M.Map String LogIndex,
    _matchIndex            :: M.Map String LogIndex,
    _storage               :: M.Map String String,
    _clientCmdQueue        :: [ClientCommand],
    _clientCmdWaitResponse :: [ClientCommand]
  } deriving (Show, Eq)

makeLenses ''NodeState

initNodeState :: NodeState
initNodeState = NodeState
  Follower  -- role
  initTerm  -- current term
  Nothing   -- voted for on this term
  []        -- log entries
  initIndex -- commit index
  initIndex -- last applied index
  Nothing   -- current known leader
  0         -- votes for me
  M.empty   -- next index of log for followers
  M.empty   -- matched index of log for followers
  M.empty   -- storage
  []        -- client commands
  []        -- client commands, waited for response

-- RPC messages
data Message =
  AppendEntries
  { _term         :: Term,
    _leaderName   :: String,
    _prevLogIndex :: LogIndex,
    _prevLogTerm  :: Term,
    _entries      :: [LogEntry],
    _leaderCommit :: LogIndex
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
  { _term          :: Term,
    _candidateName :: String,
    _lastLogIndex  :: LogIndex,
    _lastLogTerm   :: Term
  }
  | VoteForCandidate { _term :: Term }
  | DeclineCandidate { _term :: Term }
  deriving (Show, Eq, Generic, Typeable)

instance ToJSON Message
instance FromJSON Message

data MessageFrom =
  MessageFromNode
  { from    :: String,
    message :: Message
  }
  | ElectionTimeout
  | HeartbeatTimeout
  deriving (Show, Eq, Generic, Typeable)

instance ToJSON MessageFrom
instance FromJSON MessageFrom

data MessageTo =
  MessageToNode
  { to      :: String,
    message :: Message
  }
  | ClientCommandResponse
  { cmd      :: ClientCommand,
    response :: String -- TODO: add command response type
  }
  | StartElectionTimeout
  | StopElectionTimeout
  | StartHeartbeatTimout
  | StopHeartbeatTimeout
  deriving (Show, Eq, Generic, Typeable)

instance ToJSON MessageTo
instance FromJSON MessageTo

type NodeAction a = RWST Config [MessageTo] NodeState IO a
