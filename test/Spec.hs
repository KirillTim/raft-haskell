import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Lens             (set, (&), (.=), (.~), (^.))
import           Control.Monad.RWS.Strict
import           Data.List                (elemIndex)
import           Handlers
import           Types

testNodes :: Int -> [NodeInfo]
testNodes n = fmap (\x -> NodeInfo ("node"++show x) "localhost" (show $ 9000+x) True) [1..n]

testLogT42Len2 :: [LogEntry]
testLogT42Len2 = [LogEntry 0 1 (Put "k" "v"), LogEntry 1 42 (Put "k1" "v1")]

initTestConfig :: Config
initTestConfig = Config
  (head $ testNodes 3)
  (tail $ testNodes 3)

initTestNodeState = initNodeState

testFollowerHandleElectionTimeout = testCase "election timeout" $ do
  let init = initTestNodeState
  (_, st, msgs) <- runRWST handleElectionTimeout initTestConfig init
  assertBool "restart timer" $ elemIndex StopElectionTimeout msgs < elemIndex StartElectionTimeout msgs
  assertBool "request vote from node2" $ "node2" `elem` fmap to msgs
  assertBool "request vote from node3" $ "node3" `elem` fmap to msgs
  assertEqual "new state is Candidate" (st^.role) Candidate
  assertBool "increment term" $ st^.currentTerm == initNodeState^.currentTerm + 1

testFollowerHandleHeartBeatTimeout = testCase "ignore heartbeat timeout" $ do
  let init = initTestNodeState
  (_, st, msgs) <- runRWST handleHeartBeatTimeout initTestConfig init
  assertEqual "same state" st init
  assertBool "no response" $ null msgs

testFollowerHandleRequestVote = testCase "request vote" $ do -- TODO: split it
  let st = initTestNodeState & currentTerm .~ (Term 42) & eLog .~ testLogT42Len2
  let rvSmallTerm = RequestVote 20 "node2" 3 20
  (_, st1, msgs) <- runRWST (handleRequestVote "node2" rvSmallTerm) initTestConfig st
  assertEqual "decline" [MessageToNode "node2" $ DeclineCandidate 42] msgs
  assertEqual "same state" st st1
  let rvOK = RequestVote 42 "node2" 3 42
  (_, st2, msgs) <- runRWST (handleRequestVote "node2" rvOK) initTestConfig st1
  assertBool "accept" $ (MessageToNode "node2" $ VoteForCandidate 42) `elem` msgs
  assertBool "restart timer" $ elemIndex StopElectionTimeout msgs < elemIndex StartElectionTimeout msgs
  assertEqual "votedFor" (st2^.votedForOnThisTerm) (Just "node2")
  let rvVotedThisTerm = RequestVote 42 "node3" 3 42
  (_, st3, msgs) <- runRWST (handleRequestVote "node3" rvVotedThisTerm) initTestConfig st2
  assertEqual "rejected" [MessageToNode "node3" $ DeclineCandidate 42] msgs
  assertEqual "same state" st2 st3 -- TODO: custom asserts?

testFollowerHandleAppendEntries = testCase "append entries" $ do
  undefined

followerTestGroup = testGroup "Follower state tests" [testFollowerHandleElectionTimeout,
                                                       testFollowerHandleHeartBeatTimeout,
                                                       testFollowerHandleRequestVote
                                                      ]

main :: IO ()
main = defaultMain followerTestGroup
