module Utils where
import qualified Data.Map as M

import Types

buildStorage :: [LogEntry] -> M.Map String String
buildStorage entries = foldl apply M.empty $ fmap _command entries
                       where apply s cmd = case cmd of
                               Put k v  -> M.insert k v s
                               Remove k -> M.delete k s

buildTestLog :: [Command] -> [LogEntry]
buildTestLog cmds = fmap (LogEntry initIndex initTerm) cmds

testLog = buildTestLog [Put "k1" "v1", Put "k1" "v2", Remove "k2", Put "k2" "v2"]
testLog2 = testLog ++ buildTestLog [Remove "k1"]

testLog3 = []
