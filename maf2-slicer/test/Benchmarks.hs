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
    e <- generate (if manySets then genExpManySets else (arbitrary :: Gen Exp))
    i <- generate (arbitrary :: Gen Int)
    let e' = testSlice i e 
    let x = nodeCount e 
    let x' = nodeCount e'
    return $ ((x - x') / x) * 100

benchmarks' :: Bool -> Double -> IO Double
benchmarks' b 1 = benchmark b
benchmarks' b i = do 
    res <- benchmark b
    rest <- benchmarks' b (i - 1)
    return $ res + rest

benchmarks :: Bool -> Double -> IO Double
benchmarks b i = do 
    res <- benchmarks' b i 
    return $ res / i -- return the average percentage of decrease

runBenchmarks :: IO ()
runBenchmarks = do 
    let i = 100
    res <- benchmarks False i
    res' <- benchmarks True i
    putStrLn $ "after " ++ show i ++ " tests, programs get smaller by on average " ++ show res ++ "%"   
    putStrLn $ "after " ++ show i ++ " tests, programs with many sets get smaller by on average " ++ show res' ++ "%"   

testBenchmark :: IO ()
testBenchmark = do 
    es <- sample' (arbitrary :: Gen Exp)
    mapM_ printSize es

