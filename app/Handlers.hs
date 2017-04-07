{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Handlers where
import Types

import Control.Monad.RWS.Strict
import Control.Monad
import Control.Lens


handleAppendEntries :: String -> Message -> NodeAction ()
handleAppendEntries from (AppendEntries term leader prevIndex prevTerm newEntries leaderCommit) = do
  cfg <- ask
  old <- get
  liftIO $ putStrLn $ "current: " ++ show (old^.role)

  when (term < old^.currentTerm || not (logMatch (old^.eLog) prevIndex prevTerm))
    $ do tell [MessageTo from $ AppendRejected (myName cfg) (old^.currentTerm) ]

  currentLeader .= Just leader
  eLog .= logUpdate (old^.eLog) newEntries
  commitIndex .= updateCommitIndex (old^.eLog) (old^.commitIndex) leaderCommit
  lastApplied .= max (old^.commitIndex) (old^.lastApplied)
  currentTerm .= max (old^.currentTerm) term
  --becomeFollower
  votedForOnThisTerm .= Nothing --if term >= old^.currentTerm then Nothing else (old^.votedForOnThisTerm)
  role .= Follower
  tell [MessageTo from $ AppendSuccessfull (myName cfg) (old^.currentTerm) (old^.lastApplied)]
  --tell [RestartElectionTimeOut]

handleRequestVote :: String -> Message -> NodeAction ()
handleRequestVote from (RequestVote term name lastLogIndex lastLogTerm) = do
  st <- get
  when (term < st^.currentTerm || st^.votedForOnThisTerm /= Nothing)
    $ do tell [MessageTo from $ DeclineCandidate $ st^.currentTerm]
  when (isSecondAtLeastAsUpToDate (st^.eLog) [LogEntry lastLogIndex lastLogTerm (Remove "")])
    $ do
    votedForOnThisTerm .= Just name
    tell [MessageTo from $ VoteForCandidate $ st^.currentTerm]
  tell [MessageTo from $ DeclineCandidate $ st^.currentTerm]

handleElectionTimeout :: Message -> NodeAction ()
handleElectionTimeout _ = becomeCandidate

{- handleHeartBeatTimeout :: NodeAction ()
handleHeartBeatTimeout = do
  cfg <- ask
  state <- get
  mapM_ (\n -> tell [Message (_name n) $ AppendEntries (_currentTerm state) (myName cfg) (LogIndex 0) (Term 0) (_log state) (LogIndex 0)]) $ (_others cfg) -}

becomeCandidate :: NodeAction ()
becomeCandidate = do
  cfg <- ask
  currentTerm += 1
  role .= Candidate
  votedForOnThisTerm .= Just (myName cfg)
  currentLeader .= Nothing
  votesForMe .= 0

broadcast :: Message -> [NodeInfo] -> [MessageTo]
broadcast msg nodes = fmap (\n -> MessageTo (n^.name) msg) nodes

myName :: Config -> String
myName c = c^.self.name

logMatch :: [LogEntry] -> LogIndex -> Term -> Bool
logMatch log (LogIndex i) term
  | length log < i = False
  | otherwise      = t (log !! i) == term
                     where t = _term :: LogEntry -> Term

--isAtLeastAsUpToDate :: [LogEntry] -> LogIndex -> Term -> Bool
--isAtLeastAsUpToDate log index term
--  | null log           = index >= 0
--  | _term (last log)  == term = index >= ((last log)^.index)
  -- | otherwise          = term >= (last log)^.term

isSecondAtLeastAsUpToDate :: [LogEntry] -> [LogEntry] -> Bool
isSecondAtLeastAsUpToDate first second
  | null first                                = not $ null second
  | null second                               = False
  | (last first)^.term == (last second)^.term = length first <= length second
  | otherwise                                 = (last first)^.term <= (last second)^.term

logUpdate :: [LogEntry] -> [LogEntry] -> [LogEntry]
logUpdate oldLog newLog = undefined

updateCommitIndex :: [LogEntry] -> LogIndex -> LogIndex -> LogIndex
updateCommitIndex log current leader
  | leader > current = min leader $ LogIndex(length log)
  | otherwise        = current
