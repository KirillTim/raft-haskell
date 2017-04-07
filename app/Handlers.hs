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

  currentLeader .= Just from
  eLog .= logUpdate (old^.eLog) newEntries
  commitIndex .= updateCommitIndex (old^.eLog) (old^.commitIndex) leaderCommit
  lastApplied .= max (old^.commitIndex) (old^.lastApplied)
  currentTerm .= max (old^.currentTerm) term
  role .= Follower
  tell [MessageTo from $ AppendSuccessfull (myName cfg) (old^.currentTerm) (old^.lastApplied)]
  return ()

handleElectionTimeout :: Message -> NodeAction ()
handleElectionTimeout _ = becomeCandidate

{- handleHeartBeatTimeout :: NodeAction ()
handleHeartBeatTimeout = do
  cfg <- ask
  state <- get
  mapM_ (\n -> tell [Message (_name n) $ AppendEntries (_currentTerm state) (myName cfg) (LogIndex 0) (Term 0) (_log state) (LogIndex 0)]) $ (_others cfg) -}

becomeCandidate :: NodeAction ()
becomeCandidate = do
  role .= Candidate
  votedForOnThisTerm .= Nothing
  currentLeader .= Nothing
  votesForMe .= 0

myName :: Config -> String
myName c = c^.self.name

logMatch :: [LogEntry] -> LogIndex -> Term -> Bool
logMatch log (LogIndex i) term
  | length log < i = False
  | otherwise      = t (log !! i) == term
                     where t = _term :: LogEntry -> Term

logUpdate :: [LogEntry] -> [LogEntry] -> [LogEntry]
logUpdate oldLog newLog = undefined

updateCommitIndex :: [LogEntry] -> LogIndex -> LogIndex -> LogIndex
updateCommitIndex log current leader
  | leader > current = min leader $ LogIndex(length log)
  | otherwise        = current
