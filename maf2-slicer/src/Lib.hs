module Lib where

import Data.Map hiding (map)
import Lattice hiding (empty, insert)
import Data.List (intercalate)
import Interpreter.Scheme
import Dependency.State

testSto :: AbstractSto String Sign
testSto = insert "test2" ZeroOrNeg (insert "test1" STop empty)

testX :: [String]
testX = ["test1"]

printStores :: (Show v, Show k) => [AbstractSto k v] -> IO ()
printStores s = putStrLn $ intercalate "\n" (map show s)

testCovering :: IO ()
testCovering = printStores $ covering testSto

testXCovering :: IO ()
testXCovering = printStores $ xCovering testX testSto