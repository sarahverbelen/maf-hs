{-# LANGUAGE TypeApplications #-}

module Lib where

import Prelude hiding (span)

import Data.List (intercalate)
import Dependency.State
import Dependency.Dependency
import Dependency.Lattice
import Property.Preservation
import Property.Agreement
import Labels
import Syntax.Scheme.AST
import Syntax.Scheme
import Slicer

-- import Analysis.Scheme.Primitives
import Domain (inject)


import Data.Maybe
import qualified Data.Map as Map

testX :: Agreement
testX = [("x", PInt)]

testIde :: Ide 
testIde = Ide "x" NoSpan

testProgram :: String
testProgram = "programs/test.scm"

printStores :: [AbstractSto V] -> IO ()
printStores s = putStrLn $ intercalate "\n" (map show s)


printCoverings :: AbstractSto V -> IO ()
printCoverings s = do 
    let ss = covering s 
    putStrLn $ show ss
    mapM_ printCoverings ss 


printXCoverings :: AbstractSto V -> IO ()
printXCoverings s = do 
    let ss = xCoveringByProp "x" PInt s 
    putStrLn $ show ss
    mapM_ printCoverings ss     

testLabeling :: IO ()
testLabeling = do 
    contents <- readFile testProgram 
    let e = fromJust $ parseString contents
    putStrLn $ show e
    let e' =  slice e testX
    putStrLn $ show e' 
    -- putStrLn $ show $ labelSequence e testX
    -- let irrLbls = labelIrrelevant e (labelSequence e testX)
    -- putStrLn $ show $ irrLbls
    -- let usedVars = findUsedVars e irrLbls (SkipU (getVars testX))
    -- putStrLn $ show $ usedVars
    
