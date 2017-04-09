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

import Control.Lens -- (makeLenses, (+=))

data Role = Candidate | Follower | Leader deriving (Show, Eq)

newtype Term = Term Int deriving (Show, Eq, Ord, Num, Generic, Typeable)
newtype LogIndex = LogIndex Int deriving (Show, Eq, Ord, Num, Generic, Typeable)

initTerm = Term 0
initIndex = LogIndex (-1)

data Command =
  Remove { _key :: String }
  | Put { _key :: String, _value :: String}
  deriving (Show, Eq, Generic, Typeable)

makeLenses ''Command

data LogEntry = LogEntry
  { _index   :: LogIndex,
    _term    :: Term,
    _command :: Command
  } deriving (Show, Eq, Generic, Typeable)

makeLenses ''LogEntry

data NodeInfo = NodeInfo
  { _name    :: String,
    _address :: String,
    _port    :: Int,
    _isAlive :: Bool
  } deriving (Show, Eq)

makeLenses ''NodeInfo

data Config = Config
  { _self   :: NodeInfo,
    _others :: [NodeInfo]
  } deriving (Show, Eq)

makeLenses ''Config

data ClientCommand addr = ClientCommand
  { addr    :: addr,
    cmd     :: Command
  } deriving (Show, Eq, Generic, Typeable)

makeLenses ''ClientCommand

data NodeState caddr = NodeState
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
    _clientCmdQueue        :: [ClientCommand caddr],
    _clientCmdWaitResponse :: [ClientCommand caddr]
  } deriving (Show, Eq)

makeLenses ''NodeState

initNodeState :: NodeState String
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
  { _term :: Term,
    _candidateName :: String,
    _lastLogIndex  :: LogIndex,
    _lastLogTerm   :: Term
  }
  | VoteForCandidate { _term :: Term }
  | DeclineCandidate { _term :: Term }
  deriving (Show, Eq, Generic, Typeable)

data MessageFromNode =
  MessageFromNode
  { from    :: String,
    message :: Message
  }
  | ElectionTimeout
  | HeartbeatTimeout
  deriving (Show, Eq, Generic, Typeable)

data MessageTo a =
  MessageToNode
  { to      :: String,
    message :: Message
  }
  | ClientCommandResponse
  { cmd      :: ClientCommand a,
    response :: String -- TODO: add command response type
  }
  | StartElectionTimeout
  | StopElectionTimeout
  | StartHeartbeatTimout
  | StopHeartbeatTimeout
  deriving (Show, Eq, Generic, Typeable)

type MessageToStr = MessageTo String
type ClientStrAddrNodeState = NodeState String

type NodeAction a = RWST Config [MessageToStr] ClientStrAddrNodeState IO a
