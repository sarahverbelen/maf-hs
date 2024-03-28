{-# LANGUAGE FlexibleContexts, TypeApplications, ScopedTypeVariables, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Dependency.State(AbstractSto, covering, xCovering, abstractEval, abstractEvalWithState, getVarsFromExp, generateStates) where 

import Syntax.Scheme
import Lattice
import Dependency.Lattice

import qualified Data.Map as Map hiding (partition)
import Data.List (groupBy, partition)



type AbstractSto v = Map.Map Ide v

-- | TODO
abstractEval :: (RefinableLattice v) => Exp -> AbstractSto v -> v
-- | finds the value of the expression in the given abstract state
abstractEval _ _ = top

covering :: (RefinableLattice v) => AbstractSto v -> [AbstractSto v]
-- | a covering of a state s is a set of refinements of that state such that all possible values are accounted for
covering s = fmap Map.fromList (sequence $ groupBy (\ a b -> fst a == fst b) [(k, v') | (k, v) <- Map.toList s, v' <- refine v ++ [v]])

xCovering :: (RefinableLattice v) => [Ide] -> AbstractSto v -> [AbstractSto v]
-- | values of variables outside some set X take values that are the same as either the corresponding values in s or their direct subvalues
xCovering x s = fmap Map.fromList (sequence $ groupBy (\ a b -> fst a == fst b) ([(k, v') | (k, v) <- notInX, v' <- refine v ++ [v]] ++ inX))
                where (inX, notInX) = partition (\a -> elem (fst a) x) (Map.toList s)

generateStates :: (RefinableLattice v) => Exp -> AbstractSto v -> [AbstractSto v]
-- | generates a set of states from a state by extending it with the variables in the expression 
--   and computing the x-covering (keeping the variables already in the store unchanged)
generateStates e s = xCovering (Map.keys s) (extendState (getVarsFromExp e) s)

extendState :: (RefinableLattice v) => [Ide] -> AbstractSto v -> AbstractSto v 
-- | adds a list of variables to an abstract store and sets all their values to top (if they weren't in the store yet)
extendState vars sto = foldr (\var sto' -> Map.insertWith (flip const) var top sto') sto vars

abstractEvalWithState :: (RefinableLattice v) => AbstractSto v -> Exp -> v
-- | extend the abstract store with all other variables in Exp, set all of their values to Top 
--   compute the X-covering (X = all variables in the store before we extended it) of this abstract store
--   run the abstract interpreter for the expression using these stores as initial states
--   join the resulting values together to get the final value
abstractEvalWithState sto e = foldr join bottom (map (abstractEval e) (generateStates e sto))

getVarsFromExp :: Exp -> [Ide]
getVarsFromExp (Var x)             = [x]
getVarsFromExp (Iff b a c _)       = getVarsFromExp b ++ getVarsFromExp a ++ getVarsFromExp c
getVarsFromExp (Lam prs bdy _)     = prs ++ getVarsFromExp bdy
getVarsFromExp (Bgn es _)          = foldr (\e l -> l ++ getVarsFromExp e) [] es
getVarsFromExp (Dfv var e _)       = [var] ++ getVarsFromExp e
getVarsFromExp (Dff var prs bdy _) =  [var] ++ prs ++ getVarsFromExp bdy
getVarsFromExp (Set var e _)       = [var] ++ getVarsFromExp e
getVarsFromExp (Let bds bdy _)     = map fst bds ++ getVarsFromExp bdy
getVarsFromExp (Ltt bds bdy _)     = map fst bds ++ getVarsFromExp bdy
getVarsFromExp (Ltr bds bdy _)     = map fst bds ++ getVarsFromExp bdy
getVarsFromExp (Lrr bds bdy _)     = map fst bds ++ getVarsFromExp bdy
getVarsFromExp (App _ ops _)      = foldr (\e l -> l ++ getVarsFromExp e) [] ops
getVarsFromExp _                   = []

