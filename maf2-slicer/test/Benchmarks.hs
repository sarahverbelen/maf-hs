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

printSize :: Exp -> IO ()
printSize e = do 
    putStrLn $ show e 
    putStrLn $ "size before slice: " ++ (show $ nodeCount e)
    let e' = testSlice e 
    putStrLn $ "size after slice: " ++ (show $ nodeCount e')


benchmark :: IO ()
benchmark = do 
    es <- sample' (arbitrary :: Gen Exp)
    mapM_ printSize es

