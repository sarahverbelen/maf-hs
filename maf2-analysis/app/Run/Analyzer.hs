{-# LANGUAGE NamedFieldPuns, LambdaCase #-}
-- | Runs a configurable analysis on the given file
module Run.Analyzer(Options, options, main) where

import Options.Applicative
import Analysis.Scheme.Simple
import Text.Printf
import Data.List (intercalate)
import Data.Map (Map)
import Analysis.Scheme.Store (values)
import qualified Data.Map as Map

newtype Options = Options String deriving Show

printSto :: Map VariableAdr V -> String
printSto m =
   intercalate "\n" $ map (\(k,v) -> printf "%*s | %s" indent (show k) (show v)) adrs
   where adrs   = Map.toList $ Map.filterWithKey (\case { Prm _ -> const False ; _ -> const True }) m
         indent = maximum (map (length . show . fst) adrs) + 5


options :: Parser Options
options =
   Options <$>
      strOption (long "filename" <> short 'f' <> help "the file to analyse")

-- Example of a path-sensitive intraprocedural analysis.
--
-- The analysis is only intra-procedural since function calls
-- return bottom (as the runCallBottomT) and is path-sensitive
-- due to the `runNonDetT` at the bottom of the stack which
-- replaces the `runIdentity`. 
--
-- NonDetT renders the analysis path-sensitive because
-- at each branching point the analysis state is copied
-- and considered seperately. This results in an exponential
-- growth of paths in the number of branching points
-- but can result in a more precise analysis result.
--
-- evalSimple :: Exp -> [DSto K V]
-- evalSimple e = let res = Semantics.eval @_ @V e 
--                       & runEvalT
--                       & runErr
--                       & runCallBottomT
--                       & runEnv env
--                       & runSto sto
--                       & runAlloc @PaAdr (allocPai @K @_ @V)
--                       & runAlloc @VeAdr (allocVec @K @_ @V)
--                       & runAlloc @StAdr (allocStr @K @_ @V)
--                       & runAlloc @VrAdr (allocVar @K @_ @V) 
--                       & runCtx []
--                       & runNonDetT
--                in map snd res
--              where env = analysisEnvironment
--                    sto = analysisStore @V env

main :: Options -> IO ()
main (Options filename) = do
     contents <- readFile filename
     let (sto, available) = runAnalysis contents
     putStrLn $ printSto (values sto)
     print available

