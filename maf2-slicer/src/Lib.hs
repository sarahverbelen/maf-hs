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

import Analysis.Scheme.Primitives



import Data.Maybe
import qualified Data.Map as Map

testX :: Agreement
testX = [("z", PInt)]

testIde :: Ide 
testIde = Ide "x" NoSpan

testProgram :: String
testProgram = "programs/test.scm"

printStores :: [AbstractSto V] -> IO ()
printStores s = putStrLn $ intercalate "\n" (map show s)

testLabeling :: IO ()
testLabeling = do 
    contents <- readFile testProgram 
    let e = fromJust $ parseString contents
    putStrLn $ show e
    -- putStrLn $ show $ (extendStateForExp e mempty)
    -- putStrLn $ show $ abstractEval e (extendStateForExp e mempty) 
    -- putStrLn $ show $ preserveWithSto mempty testX e
    -- putStrLn $ show $ xCoveringByProp testIde PInt (extendStateForExp e mempty)
    -- putStrLn $ show $ noDep PInt (testIde, PInt) e (extendStateForExp e mempty)
    -- putStrLn $ show $ atomicExpression' 4 e PInt (extendStateForExp e mempty)
    -- putStrLn $ show $ dependencies e (Just PInt) mempty
    putStrLn $ show $ labelSequence e testX
    putStrLn $ show $ slice e testX
    -- putStrLn $ show $ labelIrrelevant e (labelSequence e testX)
