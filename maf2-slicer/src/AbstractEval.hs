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
import Analysis.Scheme.Primitives

import Data.Function ((&))
import Analysis.Monad

type AbstractSto v = Map.Map Ide v

abstractStoToEnv :: AbstractSto V -> Map.Map String Ide -- temporary: need to keep track of environment as well as store
abstractStoToEnv s = Map.union (Map.fromList $ map (\(ide, v) -> (name ide, ide)) (Map.toList s)) (initialEnv prmAdr)

abstractEval :: Exp -> AbstractSto V -> V
-- | finds the abstract value of the expression in the given abstract state
abstractEval e s = analyze' (e, abstractStoToEnv s, (), Ghost) $ fromValues s

instance (Dependency () ()) where 
       dep = undefined

instance (Dependency Ide ()) where
       dep = undefined

instance VarAdr Ide V () () where
       retAdr = undefined
       prmAdr s = Ide s NoSpan 

instance SchemeAlloc () Ide V () where
       allocVar x _ = x 
       allocCtx = undefined
       allocPai = undefined
       allocVec = undefined
       allocStr = undefined   

getValue :: MayEscape (Set DomainError) V -> V 
getValue (Value v) = v
getValue _ = bottom

analyze' :: (Exp, Map.Map String Ide, (), GT ()) -> DSto () V -> V
analyze' (exp, env, ctx, _) store =  
       let ((res, _), _) = Semantics.eval exp
              & runEvalT
              & runMayEscape @_ @(Set DomainError)
              & runCallT @V @()
              & runStoreT @VrAdr (values  store)
              & runStoreT @StAdr (strings store)
              & runStoreT @PaAdr (pairs   store)
              & runStoreT @VeAdr (vecs    store)
              & combineStores @_ @_ @_ @V
              & runEnv env
              & runAlloc @PaAdr @Exp (const $ const ())
              & runAlloc @VeAdr @Exp (const $ const ())
              & runAlloc @StAdr @Exp (const $ const ())
              & runAlloc @VrAdr @Ide @() @Ide (\from ctx -> from)
              & runCtx @() ctx
              & runIdentity      
       in getValue res