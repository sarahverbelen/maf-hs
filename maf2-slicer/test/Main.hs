module Main where 

import Spec
import Benchmarks

import Syntax.Scheme

import Test.QuickCheck

main :: IO ()
main = do 
  -- testGenSetPerc 20
  -- sample (resize 100 $ sized genExpSetPercentage)
  -- sample (resize 100 $ sized genExpSetPercentage)
  -- sample (arbitrary :: Gen Exp)
  -- sample (resize 100 $ sized genExpManySets)
  -- quickCheck (withMaxSuccess 100 prop_preserved_semantics)
  -- quickCheck (withMaxSuccess 100 prop_preserved_semantics_concrete)
  -- createBenchmarkCsv False "results/arbitrary.csv"
  -- createBenchmarkCsv True "results/forcedSets.csv"
  -- updateBenchmarkCsv "results/arbitrary.csv"
  updateBenchmarkCsv "results/forcedSets.csv"
  -- runAnalysisTimeBenchmarks "results/arbitrary.csv"
  -- runAnalysisTimeBenchmarks "results/manySets.csv"
