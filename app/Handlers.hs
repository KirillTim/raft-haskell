{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Handlers where
import Types

import Control.Monad.RWS.Strict
import Control.Monad
import Control.Lens

import qualified Data.Map as M

handleAppendEntries :: String -> Message -> NodeAction ()
handleAppendEntries from (AppendEntries term leader prevIndex prevTerm newEntries leaderCommit) = do
  cfg <- ask
  old <- get
  liftIO $ putStrLn $ "current: " ++ show (old^.role)

  when (term < old^.currentTerm || not (logMatch (old^.eLog) prevIndex prevTerm))
    $ do tell [MessageToNode from $ AppendRejected (myName cfg) (old^.currentTerm) ]

  currentLeader .= Just leader
  eLog .= logUpdate (old^.eLog) newEntries
  commitIndex .= newCommitIndex (old^.eLog) (old^.commitIndex) leaderCommit
  lastApplied .= max (old^.commitIndex) (old^.lastApplied)
  currentTerm .= max (old^.currentTerm) term
  becomeFollower
  tell [MessageToNode from $ AppendSuccessfull (myName cfg) (old^.currentTerm) (old^.lastApplied)]
  --tell [RestartElectionTimeOut]

handleAppendRejected :: String -> Message -> NodeAction ()
handleAppendRejected from (AppendRejected name term) = do
  currentT <- use currentTerm
  if currentT < term then do
    currentTerm .= term
    becomeFollower
  else do
    ni <- use nextIndex
    let Just ind = M.lookup name ni
    nextIndex .= M.insert name (ind - 1) ni
    msg <- buildAppendEntries name
    tell [MessageToNode name msg]

{-handleAppendSuccessfull :: String -> Message -> NodeAction ()
handleAppendSuccessfull from (AppendSuccessfull node term lastIndex) = do
  mi <- use matchIndex
  ni <- use nextIndex
  matchIndex .= M.insert node lastIndex mi
  nextIndex .= M.insert node (lastIndex + 1) ni
  tryUpdateCommitIndex
  ci <- use commitIndex
  la <- use lastApplied
  if (ci > la) then do
    liftIO . putStrLn "update applied index"
    lastApplied .= commitIndex
    st <- get
    let updated = updateStorage st -- TODO: fix performance
    storage .= updated -}
    

handleRequestVote :: String -> Message -> NodeAction ()
handleRequestVote from (RequestVote term name lastLogIndex lastLogTerm) = do
  st <- get
  when (term < st^.currentTerm || st^.votedForOnThisTerm /= Nothing)
    $ do tell [MessageToNode from $ DeclineCandidate $ st^.currentTerm]
  when (isSecondAtLeastAsUpToDate (st^.eLog) [LogEntry lastLogIndex lastLogTerm (Remove "")])
    ( do votedForOnThisTerm .= Just name
         tell [MessageToNode from $ VoteForCandidate $ st^.currentTerm]
         tell [StopElectionTimeout, StartElectionTimeout] -- not here ?
    )
  tell [MessageToNode from $ DeclineCandidate $ st^.currentTerm]

handleVoteForCandidate :: Message -> NodeAction ()
handleVoteForCandidate (VoteForCandidate term) = do
  ct <- use currentTerm
  r <- use role
  when (r == Candidate && term == ct) $ do
    votesForMe += 1
    have <- use votesForMe
    need <- views others $ (flip div 2) . length
    when (have > need) $ do becomeLeader

handleDeclineCandidate :: Message -> NodeAction ()
handleDeclineCandidate (DeclineCandidate term) = do
  ct <- use currentTerm
  r <- use role
  when (r == Candidate) $ do
    when (ct < term) $ do
      currentTerm .= term
      becomeFollower

handleElectionTimeout :: NodeAction ()
handleElectionTimeout = becomeCandidate

handleHeartBeatTimeout :: NodeAction ()
handleHeartBeatTimeout = do
  st <- get
  when (st^.role == Leader) $ do broadcastHeartBeat
  -- ignore otherwise

becomeLeader :: NodeAction ()
becomeLeader = do -- TODO: what to do with clientCmdQueue ?
  name <- view (self.name)
  old <- use role
  liftIO . putStrLn $ show old ++ " -> Leader"
  votedForOnThisTerm .= Nothing
  role .= Leader
  currentLeader .= Just name
  ni <- uses eLog $ (+1) . lastIndex
  otherNames <- views others (fmap _name) -- TODO: rewrite this shit
  st <- get
  mapM_ (\n -> nextIndex .= M.insert n ni (st^.nextIndex)) otherNames
  broadcastHeartBeat
  tell [StopElectionTimeout, StartHeartbeatTimout]

becomeFollower :: NodeAction ()
becomeFollower = do -- TODO: resend clientCmdQueue ?
  old <- use role
  liftIO . putStrLn $ show old ++ " -> Follower"
  votedForOnThisTerm .= Nothing
  role .= Follower
  restartElectionTimeout

becomeCandidate :: NodeAction ()
becomeCandidate = do
  cfg <- ask
  st <- get
  liftIO $ putStrLn (show (st^.role) ++ " -> Candidate")
  currentTerm += 1
  role .= Candidate
  votedForOnThisTerm .= Just (myName cfg)
  currentLeader .= Nothing
  votesForMe .= 1
  let msgToSend = broadcast (cfg^.others) $ RequestVote (st^.currentTerm) (myName cfg) (lastIndex $ st^.eLog) (lastTerm $ st^.eLog)
  mapM_ (\m -> tell [m]) msgToSend
  restartElectionTimeout

restartElectionTimeout :: NodeAction ()
restartElectionTimeout = tell [StopElectionTimeout, StartElectionTimeout]

broadcast :: [NodeInfo] -> Message -> [MessageToStr]
broadcast nodes msg = fmap (\n -> MessageToNode (n^.name) msg) nodes

broadcastHeartBeat :: NodeAction ()
broadcastHeartBeat = do
  li <- uses eLog lastIndex
  lt <- uses eLog lastTerm
  curT <- use currentTerm
  commitI <- use commitIndex
  name <- view (self.name)
  receivers <- view others
  let msgs = broadcast receivers $ AppendEntries curT name li lt [] commitI
  mapM_ (\m -> tell [m]) msgs

tryUpdateCommitIndex :: NodeAction ()
tryUpdateCommitIndex = undefined

buildAppendEntries :: String -> NodeAction Message
buildAppendEntries = undefined

updateStorage :: NodeState String -> M.Map String String
updateStorage = undefined

myName :: Config -> String
myName c = c^.self.name

-- TODO: ??? fix it. LogIndex /= position in list
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

newCommitIndex :: [LogEntry] -> LogIndex -> LogIndex -> LogIndex
newCommitIndex log current leader
  | leader > current = min leader $ LogIndex(length log)
  | otherwise        = current
