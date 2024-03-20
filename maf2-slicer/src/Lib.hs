{-# LANGUAGE TypeApplications #-}

module Lib where

import Data.Map hiding (map)
import Lattice hiding (empty, insert)
import Data.List (intercalate)
import Interpreter.Scheme
import Dependency.State
import GSystem
import Syntax.Scheme.AST
import Syntax.Scheme

import Data.Maybe

testSto :: AbstractSto Sign
-- testSto = insert "test2" ZeroOrNeg (insert "test1" STop empty)
testSto = empty

testX :: [Ide]
testX = []

testProgram = "programs/test.scm"

printStores :: (Show v) => [AbstractSto v] -> IO ()
printStores s = putStrLn $ intercalate "\n" (map show s)

testCovering :: IO ()
testCovering = printStores $ covering testSto

testXCovering :: IO ()
testXCovering = printStores $ xCovering testX testSto

testLabeling :: IO ()
testLabeling = do 
    contents <- readFile testProgram 
    let exp = fromJust $ parseString contents
    let labeledExp = labelSequence @Integer exp []
    putStrLn $ show labeledExp
