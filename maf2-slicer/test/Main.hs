module Main where 

import Spec
import Benchmarks

import Syntax.Scheme

import Test.QuickCheck

main :: IO ()
main = do 
  --sample (arbitrary :: Gen Exp)
  quickCheck (withMaxSuccess 100 prop_preserved_semantics)
  --testBenchmark
