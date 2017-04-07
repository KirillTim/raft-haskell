{-# LANGUAGE OverloadedStrings #-}
module Handlers where
import Types

import Control.Monad.RWS.Strict
import Control.Monad
import Control.Lens


handleAppendEntries :: Message -> NodeAction ()
handleAppendEntries (AppendEntries term leader prevIndex prevTerm log commit) = do
  cfg <- ask
  state <- get
  liftIO $ putStrLn $ "current: " ++ show (_role state)
  when (term < _currentTerm state) $ do tell [MessageTo "foo" $ AppendRejected (myName cfg) (_currentTerm state) ]
  --case _role state of Follower -> do
                        
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
myName c = _name $ _self c
