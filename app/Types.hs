{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Map

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Control.Lens (makeLenses)

data Role = Candidate | Follower | Leader deriving (Show, Eq)

newtype Term = Term Int deriving (Show, Eq, Ord, Generic, Typeable)
newtype LogIndex = LogIndex Int deriving (Show, Eq, Ord, Generic, Typeable)

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

data NodeName = String

data NodeState = NodeState
  { _role               :: Role,
    _config             :: Config,
    _currentTerm        :: Term,
    _votedForOnThisTerm :: Maybe NodeInfo,
    _log                :: [LogEntry],
    _commitIndex        :: LogIndex,
    _lastApplied        :: LogIndex,
    _currentLeader      :: NodeInfo,
    _votesForMe         :: Int,
    _nextIndex          :: Map NodeName LogIndex,
    _matchIndex         :: Map NodeName LogIndex
  }

makeLenses ''NodeState
