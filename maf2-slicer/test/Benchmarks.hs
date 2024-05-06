module Benchmarks where 

import Spec

import Syntax.Scheme

import Test.QuickCheck   

-- count the amount of AST nodes
nodeCount :: Exp -> Int
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

encodeData :: Exp -> Int -> String -> Exp -> Int -> String 
encodeData e x v e' x' = show e ++ ";" ++ show x ++ ";" ++ v ++ ";" ++ show e' ++ ";" ++ show x' ++ "\n"

benchmarksToCsv :: Bool -> String -> Int -> IO ()
benchmarksToCsv _ file 0 = putStrLn $ file ++ " done"
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
    benchmarksToCsv manySets filename 1000
