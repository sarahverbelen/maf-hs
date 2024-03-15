module Lib where

import Data.Map hiding (map)
import Lattice hiding (empty, insert)
import Data.List (intercalate)
import Interpreter.Scheme
import Dependency.State
import Syntax.Scheme.AST

testSto :: AbstractSto Sign
-- testSto = insert "test2" ZeroOrNeg (insert "test1" STop empty)
testSto = empty

testX :: [Ide]
testX = []

printStores :: (Show v) => [AbstractSto v] -> IO ()
printStores s = putStrLn $ intercalate "\n" (map show s)

testCovering :: IO ()
testCovering = printStores $ covering testSto

testXCovering :: IO ()
testXCovering = printStores $ xCovering testX testSto