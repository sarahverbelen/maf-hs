{-# LANGUAGE TypeApplications #-}

module Lib where

import Prelude hiding (span)

import Data.List (intercalate)
import Dependency.State
import Dependency.Lattice
import GSystem
import Syntax.Scheme.AST
import Syntax.Scheme
import Slicer

import Data.Maybe

testX :: [Ide]
testX = [Ide{name="x", span=NoSpan} ]

testProgram :: String
testProgram = "programs/test2.scm"

printStores :: [AbstractSto V] -> IO ()
printStores s = putStrLn $ intercalate "\n" (map show s)

testLabeling :: IO ()
testLabeling = do 
    contents <- readFile testProgram 
    let e = fromJust $ parseString contents
    putStrLn $ show e
    putStrLn $ show $ labelSequence e testX
    putStrLn $ show $ slice e testX
