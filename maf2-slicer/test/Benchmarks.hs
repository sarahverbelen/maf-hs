module Benchmarks where 

import Spec

import Syntax.Scheme

import Test.QuickCheck   

-- count the amount of AST nodes
nodeCount :: Exp -> Double 
nodeCount (Iff b c a _) = 1 + nodeCount b + nodeCount c + nodeCount a
nodeCount (Bgn es _) = 1 + foldr (\ a b -> b + nodeCount a) 0 es 
nodeCount (Dfv _ e _) = 1 + nodeCount e 
nodeCount (Dff _ _ e _) = 1 + nodeCount e 
nodeCount (Set _ e _) = 1 + nodeCount e 
nodeCount (Let bds bdy _) = 1 + foldr ((\ a b -> b + nodeCount a) . snd) 0 bds + nodeCount bdy
nodeCount (Ltt bds bdy _) = 1 + foldr ((\ a b -> b + nodeCount a) . snd) 0 bds + nodeCount bdy 
nodeCount (Ltr bds bdy _) = 1 + foldr ((\ a b -> b + nodeCount a) . snd) 0 bds + nodeCount bdy 
nodeCount (Lrr bds bdy _) = 1 + foldr ((\ a b -> b + nodeCount a) . snd) 0 bds + nodeCount bdy 
nodeCount (App op ops _) = 1 + nodeCount op + foldr (\ a b -> b + nodeCount a) 0 ops
nodeCount _ = 1

printSize :: Exp -> IO ()
printSize e = do 
    putStrLn $ show e 
    let x = nodeCount e
    putStrLn $ "size before slice: " ++ show x
    i <- generate (arbitrary :: Gen Int)
    let e' = testSlice i e
    let x' = nodeCount e' 
    putStrLn $ "size after slice: " ++ show x'
    let diff = ((x - x') / x) * 100 
    putStrLn $ "decreased by " ++ show diff ++ "%"

benchmark :: Bool -> IO Double 
benchmark manySets = do 
    e <- generate (if manySets then sized genExpManySets else (arbitrary :: Gen Exp))
    i <- generate (arbitrary :: Gen Int)
    let e' = testSlice i e 
    let x = nodeCount e 
    let x' = nodeCount e'
    return $ ((x - x') / x) * 100

benchmarks' :: Bool -> Double -> IO [Double]
benchmarks' b 1 = do r <- benchmark b; return [r]
benchmarks' b i = do 
    res <- benchmark b
    rest <- benchmarks' b (i - 1)
    return $ res : rest

benchmarks :: Bool -> Double -> IO (Double, Double)
benchmarks b i = do 
    vals <- benchmarks' b i 
    let average = (foldr (+) 0 vals) / i
    let std = sqrt $ (foldr (\x s -> s +  ((x - average) ^ 2)) 0 vals) / (i - 1) -- sample standard deviation
    return $ (average, std)

runBenchmarks :: IO ()
runBenchmarks = do 
    let i = 100
    (av, std) <- benchmarks False i -- random programs
    (av', std') <- benchmarks True i -- programs with a guaranteed minimum nr of set!'s
    putStrLn $ "after " ++ show i ++ " tests, programs get smaller by on average " ++ show av ++ "%" ++ ", with a standard deviation of " ++ show std ++ "%"
    putStrLn $ "after " ++ show i ++ " tests, programs with many sets get smaller by on average " ++ show av' ++ "%" ++ ", with a standard deviation of " ++ show std' ++ "%"


encodeData :: Exp -> Double -> String -> Exp -> Double -> String 
encodeData e x v e' x' = show e ++ ";" ++ show x ++ ";" ++ v ++ ";" ++ show e' ++ ";" ++ show x' ++ "\n"

benchmarksToCsv :: Bool -> String -> Int -> IO ()
benchmarksToCsv _ _ 0 = putStrLn "done"
benchmarksToCsv manySets file i = do 
    e <- generate (if manySets then sized genExpManySets else (arbitrary :: Gen Exp))
    n <- generate (arbitrary :: Gen Int)
    let var = testVar n e
    let e' = testSlice n e 
    let x = nodeCount e 
    let x' = nodeCount e'
    appendFile file $ encodeData e x var e' x'
    benchmarksToCsv manySets file (i - 1)
    
createBenchmarkCsv :: Bool -> String -> IO ()
createBenchmarkCsv manySets filename = do 
    writeFile filename "expression; size; sliced on; sliced expression; sliced size\n"
    benchmarksToCsv manySets filename 100

testBenchmark :: IO ()
testBenchmark = do 
    es <- sample' (arbitrary :: Gen Exp)
    mapM_ printSize es

