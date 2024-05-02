{-# LANGUAGE FlexibleContexts, TypeApplications, ScopedTypeVariables, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Dependency.State where 

import Syntax.Scheme
import Lattice
import Dependency.Lattice
import Property.Agreement

import Data.TypeLevel.Ghost
import Analysis.Scheme
import qualified Analysis.Scheme.Semantics as Semantics 
import Analysis.Scheme.Primitives
import Analysis.Monad
import Analysis.Scheme.Store
import Control.SVar.ModX
import Control.Monad.DomainError
import Domain.Scheme hiding (Exp, null)

import qualified Data.Map as Map hiding (partition)
import Data.List (groupBy, partition, nubBy)
import Data.Set (Set)
import Data.Functor.Identity
import Data.Function ((&))


type AbstractSto v = Map.Map String v

covering :: AbstractSto V -> [AbstractSto V]
-- | a covering of a state s is a set of refinements of that state such that all possible values are accounted for
covering s = fmap Map.fromList (filter (not . null) $ sequence $ groupBy (\ a b -> fst a == fst b) [(k, v') | (k, v) <- Map.toList s, v' <- refine v])

xCovering :: [String] -> AbstractSto V -> [AbstractSto V]
-- | values of variables outside some set X take values that are the same as either the corresponding values in s or their direct subvalues
xCovering x s = fmap Map.fromList (filter (not . null) $ sequence $ groupBy (\ a b -> fst a == fst b) ([(k, v') | (k, v) <- notInX, v' <- refine v] ++ inX))
                where (inX, notInX) = partition (\a -> elem (fst a) x) (Map.toList s)

refineByProp :: Property -> V -> [V]
refineByProp PAll v  = refine v 
refineByProp PReal v = [v { real = r }    | r <- refine $ real v]
refineByProp PInt v  = [v { integer = i } | i <- refine $ integer v]
refineByProp PBool v = [v { boolean = b } | b <- refine $ boolean v]

xCoveringByProp :: String -> Property -> AbstractSto V -> [AbstractSto V]
xCoveringByProp x PAll s = xCovering [x] s  
xCoveringByProp x p s = fmap Map.fromList (sequence $ groupBy (\ a b -> fst a == fst b) ([(k, v') | (k, v) <- notInX, v' <- refineByProp p v] ++ inX))
                where (inX, notInX) = partition (\(a, _) -> a == x) (Map.toList s) 

extendState :: [String] -> AbstractSto V -> AbstractSto V 
-- | adds a list of variables to an abstract store and sets all their values to top (if they weren't in the store yet)
extendState vars sto = foldr (\var sto' -> Map.insertWith (flip const) var top sto') sto vars

extendStateForExp :: Exp -> AbstractSto V -> AbstractSto V 
extendStateForExp e s = extendState (getVarsFromExp' e) s

generateStates :: Exp -> AbstractSto V -> [AbstractSto V]
-- | generates a set of states from a state by extending it with the variables in the expression 
--   and computing the x-covering (keeping the variables already in the store unchanged)
generateStates e s = xCovering (Map.keys s) (extendState (getVarsFromExp' e) s)

---------------------------------------------------------------------------------------------------
--- ABSTRACT EVALUATION
---------------------------------------------------------------------------------------------------

abstractEvalForCovering :: Exp -> AbstractSto V -> Value
-- | extend the abstract store with all other variables in Exp, set all of their values to Top 
--   compute the X-covering (X = all variables in the store before we extended it) of this abstract store
--   run the abstract interpreter for the expression using these stores as initial states
--   join the resulting values together to get the final value
abstractEvalForCovering e sto = 
       -- if null (getVarsFromExp' e) then foldr join bottom (map (abstractEval e) (generateStates e sto)) else error $ (show (extendState (getVarsFromExp' e) sto)) ++ show (map (abstractEval e) (generateStates e sto))
       foldr join bottom (map (abstractEval e) (generateStates e sto))

abstractStateToEnv :: Map.Map Ide V -> Map.Map String Ide -- temporary: need to keep track of environment as well as store
abstractStateToEnv s = Map.union (Map.fromList $ map (\(ide, v) -> (name ide, ide)) (Map.toList s)) (initialEnv prmAdr)

abstractStoToState :: AbstractSto V -> Map.Map Ide V 
abstractStoToState s = Map.mapKeys (\k -> Ide k NoSpan) s

abstractStateToSto :: Map.Map Ide V -> AbstractSto V 
abstractStateToSto s = Map.mapKeys name s

abstractEval' :: Exp -> AbstractSto V -> (Value, AbstractSto V)
-- | version of abstractEval that also returns the updated store
abstractEval' e s = (v, abstractStateToSto $ values sto') 
       where  (v, sto') = analyze'' (e, env, (), Ghost) sto
              s' = abstractStoToState $ extendStateForExp e s
              env = abstractStateToEnv s'
              sto = fromValues $ Map.union (abstractStoToState s) (initialSto env)

abstractEval :: Exp -> AbstractSto V -> Value
-- | finds the abstract value of the expression in the given abstract state
abstractEval e s = fst $ abstractEval' e s
                            
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
getValue v = bottom

type Value = MayEscape (Set DomainError) V

analyze' :: (Exp, Map.Map String Ide, (), GT ()) -> DSto () V -> Value
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
       in res --getValue res

analyze'' :: (Exp, Map.Map String Ide, (), GT ()) -> DSto () V -> (Value, DSto () V)
analyze'' (exp, env, ctx, _) store =  
       let ((res, _), sto) = Semantics.eval exp
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
       in (res, sto) --(getValue res, sto)       


-- auxiliary function that extracts the variables used in an expression

getVarsFromExp' :: Exp -> [String]
-- version where the variables are unique by name
getVarsFromExp' e = map name $ (nubBy (\a b -> name a == name b)) $ getVarsFromExp e

getVarsFromExp :: Exp -> [Ide]
getVarsFromExp (Var x)             = [x]
getVarsFromExp (Iff b a c _)       = getVarsFromExp b ++ getVarsFromExp a ++ getVarsFromExp c
getVarsFromExp (Lam prs bdy _)     = prs ++ getVarsFromExp bdy
getVarsFromExp (Bgn es _)          = foldr (\e l -> l ++ getVarsFromExp e) [] es
getVarsFromExp (Dfv var e _)       = [var] ++ getVarsFromExp e
getVarsFromExp (Dff var prs bdy _) = [var] ++ prs ++ getVarsFromExp bdy
getVarsFromExp (Set var e _)       = [var] ++ getVarsFromExp e
getVarsFromExp (Let bds bdy _)     = map fst bds ++ getVarsFromExp bdy
getVarsFromExp (Ltt bds bdy _)     = map fst bds ++ getVarsFromExp bdy
getVarsFromExp (Ltr bds bdy _)     = map fst bds ++ getVarsFromExp bdy
getVarsFromExp (Lrr bds bdy _)     = map fst bds ++ getVarsFromExp bdy
getVarsFromExp (App _ ops _)       = foldr (\e l -> l ++ getVarsFromExp e) [] ops
getVarsFromExp _                   = []

