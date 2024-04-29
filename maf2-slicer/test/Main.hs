module Main where 

import Spec
import Benchmarks

import Syntax.Scheme

import Test.QuickCheck

main :: IO ()
main = do 
  --sample (arbitrary :: Gen Exp)
  --quickCheck (withMaxSuccess 100 prop_preserved_semantics)
  res <- runBenchmarks 500 
  putStrLn $ "after 500 tests, programs get smaller by on average " ++ show res ++ "%"
