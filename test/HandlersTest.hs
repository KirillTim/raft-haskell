module HandlersTest where
import Test.Tasty
import Test.Tasty.HUnit

import Types
import Handlers
import Control.Monad.RWS.Strict
import Data.List (elemIndex)
import Control.Lens ((^.))

testNodes :: Int -> [NodeInfo]
testNodes n = fmap (\x -> NodeInfo ("node"++show x) "localhost" (9000+x) True) [1..n]

initTestConfig :: Config
initTestConfig = Config
  (head $ testNodes 3)
  (tail $ testNodes 3)

initTestNodeState = initNodeState

testFollowerHandleElectionTimeout = testCase "election timeout" $ do
  (_, st, msgs) <- runRWST handleElectionTimeout initTestConfig initTestNodeState
  assertBool "restart timer" $ elemIndex StopElectionTimeout msgs < elemIndex StartElectionTimeout msgs
  assertBool "request vote from node2" $ "node2" `elem` fmap to msgs
  assertBool "request vote from node3" $ "node3" `elem` fmap to msgs
  assertBool "new state is Candidate" $ st^.role == Candidate
  assertBool "increment term" $ st^.currentTerm == initNodeState^.currentTerm + 1

followerTestGroup =  testGroup "Follower state tests" [testFollowerHandleElectionTimeout
                                                      ]

main :: IO ()
main = defaultMain followerTestGroup
