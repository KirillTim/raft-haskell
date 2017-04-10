module Utils where
import qualified Data.Map as M (Map, delete, empty, insert)

import           Types    (Command (..), LogEntry (..), initIndex, initTerm)

buildStorage :: [LogEntry] -> M.Map String String
buildStorage entries = foldl apply M.empty $ fmap _command entries
                       where apply s cmd = case cmd of
                               Put k v  -> M.insert k v s
                               Remove k -> M.delete k s

buildTestLog :: [Command] -> [LogEntry]
buildTestLog = fmap (LogEntry initIndex initTerm)

testLog :: [LogEntry]
testLog = buildTestLog [Put "k1" "v1", Put "k1" "v2", Remove "k2", Put "k2" "v2"]

testLog2 :: [LogEntry]
testLog2 = testLog ++ buildTestLog [Remove "k1"]

testLog3 :: [LogEntry]
testLog3 = []
