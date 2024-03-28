{-# LANGUAGE FlexibleContexts, TypeApplications, ScopedTypeVariables, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module AbstractEval(abstractEval, AbstractSto) where 

import Syntax.Scheme
import Lattice
import Dependency.Lattice

import qualified Data.Map as Map hiding (partition)
import Data.List (groupBy, partition)
import Data.Set (Set)

import Data.TypeLevel.Ghost
import Analysis.Scheme
import Control.SVar.ModX
import Analysis.Scheme.Store
import Domain.Scheme hiding (Exp)
import Control.Monad.DomainError

import Data.Functor.Identity
import qualified Analysis.Scheme.Semantics as Semantics 

import Data.Function ((&))
import Analysis.Monad

type AbstractSto v = Map.Map Ide v

abstractStoToEnv :: AbstractSto V -> Map.Map String Ide -- temporary: need to keep track of environment as well as store
abstractStoToEnv s = Map.fromList $ map (\(ide, v) -> (name ide, ide)) (Map.toList s)

abstractEval :: Exp -> AbstractSto V -> V
-- | finds the value of the expression in the given abstract state
abstractEval e s = analyze' (e, (abstractStoToEnv s), (), Ghost) $ fromValues s

instance (Dependency () ()) where 
       dep = undefined

instance (Dependency Ide ()) where
       dep = undefined

instance VarAdr Ide V () () where
       retAdr = undefined
       prmAdr = undefined  

instance SchemeAlloc () Ide V () where
       allocVar x _ = x 
       allocCtx = undefined
       allocPai = undefined
       allocVec = undefined
       allocStr = undefined   

analyze' :: (Exp, Map.Map String Ide, (), GT ()) -> DSto () V -> V
analyze' (exp, env, ctx, _) store = 
       let ((Value v, (spawns, registers, triggers)), sto) = Semantics.eval exp
              & runEvalT
              & runMayEscape @_ @(Set DomainError)
              & runCallT @V @()
              & runStoreT @VrAdr @_ @V (values  store)
              & runStoreT @StAdr @_ @(StrDom V) (strings store)
              & runStoreT @PaAdr @_ @(PaiDom V) (pairs   store)
              & runStoreT @VeAdr @_ @(VecDom V) (vecs    store)
              & combineStores @_ @_ @_ @V
              & runEnv env
              & runAlloc @PaAdr undefined
              & runAlloc @VeAdr undefined
              & runAlloc @StAdr undefined
              & runAlloc @VrAdr (const id)
              & runCtx  ctx
              & runIdentity
       in v      