{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Benchmarks where 

import Spec

import Syntax.Scheme
import Analysis.Scheme.Simple --(runAnalysis)

import Test.QuickCheck   
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Map (Map)
import Analysis.Scheme.Store (values)
import qualified Data.Map as Map
import Text.Printf
import System.Clock

encodeData :: Exp -> Integer -> Int -> Exp -> Integer -> Integer -> String 
encodeData e x v e' x' t = show e ++ ";" ++ show (setPercentage e) ++ ";" ++ show x ++ ";" ++ show v ++ ";" ++ show e' ++ ";" ++ show x' ++ ";" ++ show t ++ "\n"

appendData :: String -> Exp -> Integer -> Integer -> String
appendData s e i t = s ++ ";" ++ show e ++ ";" ++ show i ++ ";" ++ show t ++ "\n"

benchmark :: Exp -> Int -> IO (Exp, Integer) 
benchmark e n = do
    let var = testVar n e 
    let e' = concreteTestSlice n e 
    let x = nodeCount e' 
    return (e', x)

benchmarksToCsv :: Bool -> String -> Int -> IO ()
benchmarksToCsv _ file 0 = putStrLn $ file ++ " done"
benchmarksToCsv manySets file i = do 
    e <- generate (if manySets then genExpSetPercentage 20 100 else (arbitrary :: Gen Exp))
    n <- generate (arbitrary :: Gen Int)
    (t, (e', x')) <- timeInNs $ benchmark e n
    let x = nodeCount e 
    appendFile file $ encodeData e x n e' x' t
    putStrLn $ "written " ++ show i ++ " after " ++ show t
    benchmarksToCsv manySets file (i - 1)
    
createBenchmarkCsv :: Bool -> FilePath -> IO ()
createBenchmarkCsv manySets filename = do 
    -- writeFile filename "expression; %set!s; size; sliced on; sign slice; sign size; sign time (ns) \n"
    benchmarksToCsv manySets filename 24

updateBenchmarkCsv :: FilePath -> IO ()
updateBenchmarkCsv filename = do 
    contents <- readFile filename 
    putStrLn contents -- force the contents to be read completely so we can write to the file
    let (h:dataLines) = lines contents
    let newH = h ++ "; concrete slice; concrete size; concrete time \n"
    writeFile filename newH 
    mapM_ (updateBenchmark filename) dataLines
    putStrLn $ "done updating " ++ filename 

updateBenchmark :: FilePath -> String -> IO () 
updateBenchmark filename d = do 
    let splitData = map T.unpack $ T.splitOn (T.pack ";") (T.pack d) 
    let e = fromJust $ parseString $ splitData !! 0 
    let n = read $ splitData !! 2 
    (t, (e', x')) <- timeInNs $ benchmark e n 
    appendFile filename (appendData d e' x' t) 
    putStrLn $ "updated record after " ++ show t ++ "ns"

runAnalysisTimeBenchmarks :: FilePath -> IO ()
runAnalysisTimeBenchmarks filename = do 
    contents <- readFile filename 
    putStrLn contents -- force the contents to be read completely so we can write to the file 
    let (h:dataLines) = lines contents 
    let newH = h ++ "; abstract analysis time \n"
    writeFile filename newH 
    mapM_ (benchmarkAnalysisTime filename) dataLines 
    putStrLn $ "done benchmarking analysis times in " ++ filename

benchmarkAnalysisTime :: FilePath -> String -> IO ()
benchmarkAnalysisTime filename d = do 
    let splitData = map T.unpack $ T.splitOn (T.pack ";") (T.pack d)
    let e = splitData !! 6
    (t, _) <- timeInNs $ runAnalysisNTimes e 100
    appendFile filename (d ++ ";" ++ show t ++ "\n")
    putStrLn $ "analysis took " ++ show t ++ "ns"

runAnalysisNTimes :: String -> Int -> IO ()
runAnalysisNTimes _ 0 = return ()
runAnalysisNTimes s n = do 
    analyze s 
    runAnalysisNTimes s (n - 1)


analyze :: String -> IO ()
analyze contents = do 
   putStrLn $ printSto (values (runAnalysis contents))

printSto :: Map VariableAdr V -> String
printSto m =
   intercalate "\n" $ map (\(k,v) -> printf "%*s | %s" indent (show k) (show v)) adrs
   where adrs   = Map.toList $ Map.filterWithKey (\case { Prm _ -> const False ; _ -> const True }) m
         indent = maximum (map (length . show . fst) adrs) + 5

-- modified from https://hackage.haskell.org/package/timeit-1.0.0.0/docs/src/System-TimeIt.html#timeItT to return time in ns
timeInNs :: IO a -> IO (Integer, a)
timeInNs ioa = do
    t1 <- getTime Monotonic
    a <- ioa
    t2 <- getTime Monotonic
    let t = toNanoSecs $ diffTimeSpec t2 t1
    return (t, a)