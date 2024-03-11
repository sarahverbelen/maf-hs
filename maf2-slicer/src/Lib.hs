module Lib where

import Data.Map hiding (map)
import Lattice hiding (empty, insert)
import Data.List (intercalate)
import Interpreter.Scheme
import Dependency.State

testSto :: AbstractSto Sign
testSto = insert (PrmAdr "test2") ZeroOrNeg (insert (PrmAdr "test1") STop empty)

testX :: [CAdr]
testX = [PrmAdr "test1"]

printStores :: (Show v) => [AbstractSto v] -> IO ()
printStores s = putStrLn $ intercalate "\n" (map show s)

testCovering :: IO ()
testCovering = printStores $ covering testSto

testXCovering :: IO ()
testXCovering = printStores $ xCovering testX testSto