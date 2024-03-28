{-# LANGUAGE TypeApplications #-}

module Lib where

import Prelude hiding (span)

import Data.List (intercalate)
import Dependency.State
import GSystem
import Syntax.Scheme.AST
import Syntax.Scheme
import Lattice

import Data.Maybe

testX :: [Ide]
testX = [Ide{name="x", span=Span{filename="SExpParser", line=1, column=1}} ]

testProgram :: String
testProgram = "programs/test2.scm"

printStores :: (Show v) => [AbstractSto v] -> IO ()
printStores s = putStrLn $ intercalate "\n" (map show s)

testLabeling :: IO ()
testLabeling = do 
    contents <- readFile testProgram 
    let e = fromJust $ parseString contents
    let labeledExp = labelSequence @Sign e testX
    putStrLn $ show e
    putStrLn $ show labeledExp
