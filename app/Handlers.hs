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
  becomeFollower
  tell [MessageTo from $ AppendSuccessfull (myName cfg) (old^.currentTerm) (old^.lastApplied)]
  --tell [RestartElectionTimeOut]

handleRequestVote :: String -> Message -> NodeAction ()
handleRequestVote from (RequestVote term name lastLogIndex lastLogTerm) = do
  st <- get
  when (term < st^.currentTerm || st^.votedForOnThisTerm /= Nothing)
    $ do tell [MessageTo from $ DeclineCandidate $ st^.currentTerm]
  when (isSecondAtLeastAsUpToDate (st^.eLog) [LogEntry lastLogIndex lastLogTerm (Remove "")])
    ( do votedForOnThisTerm .= Just name
         tell [MessageTo from $ VoteForCandidate $ st^.currentTerm]
         --tell [RestartElectionTimeOut]
    )
  tell [MessageTo from $ DeclineCandidate $ st^.currentTerm]

handleElectionTimeout :: NodeAction ()
handleElectionTimeout = do
  st <- get
  when (st^.role == Candidate) $ do becomeCandidate
  -- ignore otherwise

handleHeartBeatTimeout :: NodeAction ()
handleHeartBeatTimeout = do
  st <- get
  when (st^.role == Leader) $ do broadcastHeartBeat
  -- ignore otherwise

becomeLeader :: NodeAction ()
becomeLeader = do
  cfg <- ask
  st <- get
  liftIO $ putStrLn (show (st^.role) ++ "-> Leader")
  votedForOnThisTerm .= Nothing
  role .= Leader
  currentLeader .= Just (myName cfg)
  broadcastHeartBeat
  tell [MessageTo "" StopElectionTimeout, MessageTo "" StartElectionTimeout]


becomeFollower :: NodeAction ()
becomeFollower = do
  st <- get
  liftIO $ putStrLn (show (st^.role) ++ "-> Follower")
  votedForOnThisTerm .= Nothing
  role .= Follower

becomeCandidate :: NodeAction ()
becomeCandidate = do
  cfg <- ask
  st <- get
  liftIO $ putStrLn (show (st^.role) ++ "-> Candidate")
  currentTerm += 1
  role .= Candidate
  votedForOnThisTerm .= Just (myName cfg)
  currentLeader .= Nothing
  votesForMe .= 1
  let msgToSend = broadcast (cfg^.others) $ RequestVote (st^.currentTerm) (myName cfg) (lastIndex $ st^.eLog) (lastTerm $ st^.eLog)
  mapM_ (\m -> tell [m]) msgToSend

broadcast :: [NodeInfo] -> Message -> [MessageTo]
broadcast nodes msg = fmap (\n -> MessageTo (n^.name) msg) nodes

broadcastHeartBeat :: NodeAction ()
broadcastHeartBeat = do
  cfg <- ask
  st <- get
  let msgToSend = broadcast (cfg^.others) $ AppendEntries (st^.currentTerm) (myName cfg) (lastIndex $ st^.eLog) (lastTerm $ st^.eLog) [] (st^.commitIndex)
  mapM_ (\m -> tell [m]) msgToSend


myName :: Config -> String
myName c = c^.self.name

logMatch :: [LogEntry] -> LogIndex -> Term -> Bool
logMatch log (LogIndex i) term
  | length log < i = False
  | otherwise      = t (log !! i) == term
                     where t = _term :: LogEntry -> Term

isSecondAtLeastAsUpToDate :: [LogEntry] -> [LogEntry] -> Bool
isSecondAtLeastAsUpToDate first second
  | lastTerm second > lastTerm first  = True
  | lastTerm second == lastTerm first = lastIndex second >= lastIndex first
  | otherwise                         = False

lastIndex :: [LogEntry] -> LogIndex
lastIndex []  = initIndex
lastIndex log = i (last log)
                where i = _index :: LogEntry -> LogIndex

lastTerm :: [LogEntry] -> Term
lastTerm []  = initTerm
lastTerm log = (last log)^.term


logUpdate :: [LogEntry] -> [LogEntry] -> [LogEntry]
logUpdate oldLog newLog = undefined

updateCommitIndex :: [LogEntry] -> LogIndex -> LogIndex -> LogIndex
updateCommitIndex log current leader
  | leader > current = min leader $ LogIndex(length log)
  | otherwise        = current
