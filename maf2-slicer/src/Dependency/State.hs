{-# LANGUAGE FlexibleContexts, TypeApplications, ScopedTypeVariables, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Dependency.State(AbstractSto, covering, xCovering, abstractEval, abstractEvalWithPredicate) where 

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
import Data.Map (Map)
import Data.Functor.Identity
import Data.TypeLevel.Ghost
import Data.Function ((&))
import Data.Map hiding (partition)
import Data.List (groupBy, partition, elem)
import Analysis.Monad (EnvM(..))
import Analysis.Scheme.Store
import Control.Monad.DomainError (runMayEscape, DomainError, MonadEscape(..))

import Dependency.Lattice
import Analysis.Scheme

type AbstractSto v = Map Ide v

covering :: (RefinableLattice v) => AbstractSto v -> [AbstractSto v]
-- | a covering of a state s is a set of refinements of that state such that all possible values are accounted for
covering s = fmap fromList (sequence $ groupBy (\ a b -> fst a == fst b) [(k, v') | (k, v) <- toList s, v' <- refine v ++ [v]])

xCovering :: (RefinableLattice v) => [Ide] -> AbstractSto v -> [AbstractSto v]
-- | values of variables outside some set X take values that are the same as either the corresponding values in s or their direct subvalues
xCovering x s = fmap fromList (sequence $ groupBy (\ a b -> fst a == fst b) ([(k, v') | (k, v) <- notInX, v' <- refine v ++ [v]] ++ inX))
                where (inX, notInX) = partition (\a -> elem (fst a) x) (toList s)

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


abstractEval :: Exp -> AbstractSto v -> (AbstractSto v, v)
abstractEval = undefined

abstractEvalWithPredicate :: (AbstractSto v -> Bool) -> Exp -> (AbstractSto v, v)
-- generate an abstract store that containts all variables in Exp, set all of their values to Top 
-- compute the covering of this abstract store (one lvl or for all lvls?)
-- (filter the set of stores using the predicate)
-- run the abstract interpreter for the expression using these stores as initial states
abstractEvalWithPredicate = undefined