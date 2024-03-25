{-# LANGUAGE FlexibleContexts, TypeApplications, ScopedTypeVariables, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Dependency.State(AbstractSto, covering, xCovering, abstractEval, abstractEvalWithState, getVarsFromExp) where 

import Analysis.Scheme.Primitives
import qualified Analysis.Scheme.Semantics as Semantics
import Analysis.Scheme.Monad (SchemeM)
import Analysis.Monad hiding (getEnv)

import Control.SVar.ModX
import Control.Monad.Trans.Class
import Control.Monad.Join
import Control.Monad.Layer
import Control.Monad.Cond (whenM)

import Syntax.Scheme
import Lattice
import Domain.Scheme hiding (Exp, Env)

import Data.Set (Set)
import Data.Functor.Identity
import Data.TypeLevel.Ghost
import Data.Function ((&))
import qualified Data.Map as Map hiding (partition)
import Data.List (groupBy, partition, elem)
import Analysis.Monad (EnvM(..))
import Analysis.Scheme.Store
import Control.Monad.DomainError (runMayEscape, DomainError, MonadEscape(..))

import Dependency.Lattice
import Analysis.Scheme

type AbstractSto v = Map.Map Ide v

covering :: (RefinableLattice v) => AbstractSto v -> [AbstractSto v]
-- | a covering of a state s is a set of refinements of that state such that all possible values are accounted for
covering s = fmap Map.fromList (sequence $ groupBy (\ a b -> fst a == fst b) [(k, v') | (k, v) <- Map.toList s, v' <- refine v ++ [v]])

xCovering :: (RefinableLattice v) => [Ide] -> AbstractSto v -> [AbstractSto v]
-- | values of variables outside some set X take values that are the same as either the corresponding values in s or their direct subvalues
xCovering x s = fmap Map.fromList (sequence $ groupBy (\ a b -> fst a == fst b) ([(k, v') | (k, v) <- notInX, v' <- refine v ++ [v]] ++ inX))
                where (inX, notInX) = partition (\a -> elem (fst a) x) (Map.toList s)

----
--     abstractEval :: (Exp, Env var v ctx dep, ctx) -> DSto ctx v -> (DSto ctx v, v)
--     -- abstractEval (exp, env, ctx) store =
--     --    let ((val, (spawns, registers, triggers)), sto) = (Semantics.eval exp >>= writeAdr (retAdr (exp, env, ctx, Ghost)))
--     --           & runEvalT
--     --           & runMayEscape @_ @(Set DomainError)
--     --           & runCallT @v @ctx
--     --           & runStoreT @VrAdr (values  store)
--     --           & runStoreT @StAdr (strings store)
--     --           & runStoreT @PaAdr (pairs   store)
--     --           & runStoreT @VeAdr (vecs    store)
--     --           & combineStores
--     --           & runEnv env
--     --           & runAlloc @PaAdr (allocPai @ctx)
--     --           & runAlloc @VeAdr (allocVec @ctx)
--     --           & runAlloc @StAdr (allocStr @ctx)
--     --           & runAlloc @VrAdr (allocVar @ctx)
--     --           & runCtx  ctx
--     --           & runIdentity
--     --    in (sto, val) 


abstractEval :: AbstractSto v -> Exp -> v
-- finds the value of the expression in the given abstract state
abstractEval = undefined

generateStates :: (RefinableLattice v) => Exp -> AbstractSto v -> [AbstractSto v]
-- generates a set of states where 
generateStates e s = xCovering varsInExp (extendState varsInExp s) where varsInExp = (getVarsFromExp e)

extendState :: (RefinableLattice v) => [Ide] -> AbstractSto v -> AbstractSto v 
extendState vars sto = foldr (\var sto' -> Map.insert var top sto') sto vars

abstractEvalWithState :: (RefinableLattice v) => AbstractSto v -> Exp -> v
-- extend the abstract store with all other variables in Exp, set all of their values to Top 
-- compute the X-covering (X = all variables in the store before we extended it) of this abstract store (one lvl or for all lvls?)
-- run the abstract interpreter for the expression using these stores as initial states
abstractEvalWithState sto e = abstractEval (head $ generateStates e sto) e


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
getVarsFromExp (App op ops _)      = foldr (\e l -> l ++ getVarsFromExp e) [] ops -- ++ getVarsFromExp op
getVarsFromExp _                   = []