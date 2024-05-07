module Main where 

import Spec
import Benchmarks

import Syntax.Scheme

import Test.QuickCheck

main :: IO ()
main = do 
  -- sample (arbitrary :: Gen Exp)
  -- sample (resize 100 $ sized genExpManySets)
  -- quickCheck (withMaxSuccess 100 prop_preserved_semantics)
  -- createBenchmarkCsv False "results/arbitrary.csv"
  createBenchmarkCsv True "results/manySets.csv"
  -- updateBenchmarkCsv "results/arbitrary.csv"
  -- updateBenchmarkCsv "results/manySets.csv"
