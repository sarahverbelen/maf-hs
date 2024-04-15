{-# LANGUAGE TypeApplications #-}

module Lib where

import Prelude hiding (span)

import Data.List (intercalate)
import Dependency.State
import Dependency.Dependency
import Dependency.Lattice
-- import Property.Preservation
import Property.Agreement
-- import Labels
import Syntax.Scheme.AST
import Syntax.Scheme
import Slicer

-- import Analysis.Scheme.Primitives



import Data.Maybe
-- import qualified Data.Map as Map

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

testLabeling :: IO ()
testLabeling = do 
    contents <- readFile testProgram 
    let e = fromJust $ parseString contents
    -- let e' =  slice e testX
    -- putStrLn $ show e
    -- putStrLn $ show e' 
    -- printCoverings (extendStateForExp e mempty)
    -- putStrLn $ show $ (extendStateForExp e mempty)
    -- putStrLn $ show $ abstractEval e mempty 
    -- putStrLn $ show $ preserveWithSto mempty testX e
    -- putStrLn $ show $ xCoveringByProp testIde PInt (extendStateForExp e mempty)
    -- putStrLn $ show $ noDep' 16 PInt (testIde, PInt) e (extendStateForExp e mempty)
    -- putStrLn $ show $ atomicExpression' 4 e PInt (extendStateForExp e mempty)
    putStrLn $ show $ findNDeps e (Just PInt) (extendStateForExp e mempty)
    -- putStrLn $ show $ labelSequence e testX
    -- putStrLn $ show $ slice e testX
    -- putStrLn $ show $ labelIrrelevant e (labelSequence e testX)
