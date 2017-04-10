{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Handlers where
import           Types                    (Command (..), Config (..),
                                           LogEntry (..), LogIndex (..),
                                           Message (..), MessageTo (..),
                                           NodeAction, NodeInfo (..),
                                           NodeState (..), Role (..), Term (..),
                                           commitIndex, currentLeader,
                                           currentTerm, eLog, initIndex,
                                           initTerm, lastApplied, name,
                                           nextIndex, others, role, self, term,
                                           votedForOnThisTerm, votesForMe)

import           Control.Lens             (use, uses, view, views, (+=), (.=),
                                           (^.))
import           Control.Monad            (when)
import           Control.Monad.RWS.Strict (ask, get, liftIO, tell)

import qualified Data.Map                 as M (Map, insert, lookup)

handleAppendEntries :: String -> Message -> NodeAction ()
handleAppendEntries from (AppendEntries mTerm leader prevIndex prevTerm newEntries leaderCommit) = do
  cfg <- ask
  old <- get
  liftIO $ putStrLn $ "current: " ++ show (old^.role)

  when (mTerm < old^.currentTerm || not (logMatch (old^.eLog) prevIndex prevTerm))
    $ tell [MessageToNode from $ AppendRejected (myName cfg) (old^.currentTerm)]

  currentLeader .= Just leader
  eLog .= logUpdate (old^.eLog) newEntries
  commitIndex .= newCommitIndex (old^.eLog) (old^.commitIndex) leaderCommit
  lastApplied .= max (old^.commitIndex) (old^.lastApplied)
  currentTerm .= max (old^.currentTerm) mTerm
  becomeFollower
  tell [MessageToNode from $ AppendSuccessfull (myName cfg) (old^.currentTerm) (old^.lastApplied)]
  --tell [RestartElectionTimeOut]

handleAppendEntries _ _ = undefined

handleAppendRejected :: String -> Message -> NodeAction ()
handleAppendRejected _ (AppendRejected mName mTerm) = do
  currentT <- use currentTerm
  if currentT < mTerm then do
    currentTerm .= mTerm
    becomeFollower
  else do
    ni <- use nextIndex
    let Just ind = M.lookup mName ni
    nextIndex .= M.insert mName (ind - 1) ni
    msg <- buildAppendEntries mName
    tell [MessageToNode mName msg]

handleAppendRejected _ _ = undefined

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
handleRequestVote from (RequestVote mTerm mName lastLogIndex lastLogTerm) = do
  ct <- use currentTerm
  vote <- use votedForOnThisTerm
  entries <- use eLog
  if (mTerm < ct || vote /= Nothing) then do
    tell [MessageToNode from $ DeclineCandidate ct]
  else do
    when (isSecondAtLeastAsUpToDate entries [LogEntry lastLogIndex lastLogTerm (Remove "")]) $ do
      votedForOnThisTerm .= Just mName
      tell [MessageToNode from $ VoteForCandidate $ ct]
      restartElectionTimeout

handleRequestVote _ _ = undefined

handleVoteForCandidate :: Message -> NodeAction ()
handleVoteForCandidate (VoteForCandidate mTerm) = do
  ct <- use currentTerm
  r <- use role
  when (r == Candidate && mTerm == ct) $ do
    votesForMe += 1
    have <- use votesForMe
    need <- views others $ (flip div 2) . length
    when (have > need) $ do becomeLeader

handleVoteForCandidate _ = undefined

handleDeclineCandidate :: Message -> NodeAction ()
handleDeclineCandidate (DeclineCandidate mTerm) = do
  ct <- use currentTerm
  r <- use role
  when (r == Candidate) $ do
    when (ct < mTerm) $ do
      currentTerm .= mTerm
      becomeFollower

handleDeclineCandidate _ = undefined

handleElectionTimeout :: NodeAction ()
handleElectionTimeout = becomeCandidate

handleHeartBeatTimeout :: NodeAction ()
handleHeartBeatTimeout = do
  st <- get
  when (st^.role == Leader) $ do broadcastHeartBeat
  -- ignore otherwise

becomeLeader :: NodeAction ()
becomeLeader = do -- TODO: what to do with clientCmdQueue ?
  selfName <- view (self.name)
  old <- use role
  liftIO . putStrLn $ show old ++ " -> Leader"
  votedForOnThisTerm .= Nothing
  role .= Leader
  currentLeader .= Just selfName
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

broadcast :: [NodeInfo] -> Message -> [MessageTo]
broadcast nodes msg = fmap (\n -> MessageToNode (n^.name) msg) nodes

broadcastHeartBeat :: NodeAction ()
broadcastHeartBeat = do
  li <- uses eLog lastIndex
  lt <- uses eLog lastTerm
  curT <- use currentTerm
  commitI <- use commitIndex
  selfName <- view (self.name)
  receivers <- view others
  let msgs = broadcast receivers $ AppendEntries curT selfName li lt [] commitI
  mapM_ (\m -> tell [m]) msgs

tryUpdateCommitIndex :: NodeAction ()
tryUpdateCommitIndex = undefined

buildAppendEntries :: String -> NodeAction Message
buildAppendEntries = undefined

updateStorage :: NodeState -> M.Map String String
updateStorage = undefined

myName :: Config -> String
myName c = c^.self.name

-- TODO: ??? fix it. LogIndex /= position in list
logMatch :: [LogEntry] -> LogIndex -> Term -> Bool
logMatch entries (LogIndex i) iTerm
  | length entries < i = False
  | otherwise      = t (entries !! i) == iTerm
                     where t = _term :: LogEntry -> Term

isSecondAtLeastAsUpToDate :: [LogEntry] -> [LogEntry] -> Bool
isSecondAtLeastAsUpToDate first second
  | lastTerm second > lastTerm first  = True
  | lastTerm second == lastTerm first = lastIndex second >= lastIndex first
  | otherwise                         = False

lastIndex :: [LogEntry] -> LogIndex
lastIndex []  = initIndex
lastIndex entries = i (last entries)
                where i = _index :: LogEntry -> LogIndex

lastTerm :: [LogEntry] -> Term
lastTerm []      = initTerm
lastTerm entries = (last entries)^.term


logUpdate :: [LogEntry] -> [LogEntry] -> [LogEntry]
logUpdate = undefined

newCommitIndex :: [LogEntry] -> LogIndex -> LogIndex -> LogIndex
newCommitIndex entries current leader
  | leader > current = min leader $ LogIndex(length entries)
  | otherwise        = current
