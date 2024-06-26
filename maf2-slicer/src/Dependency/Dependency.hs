module Dependency.Dependency where 

import Dependency.Lattice 
import Dependency.State
import Property.Agreement

import Data.List ((\\), union)
import Control.Monad.State

import Syntax.Scheme.AST
import Domain.Scheme hiding (Exp, null)
import qualified Control.Monad.Escape as Escape

atomByProp' :: Property -> V -> Bool 
atomByProp' PAll  v = atom v
atomByProp' PInt  v = atom $ integer v 
atomByProp' PBool v = atom $ boolean v

atomByProp :: Property -> Value -> Bool 
atomByProp p (Escape.Value v) = atomByProp' p v
atomByProp p (Escape.MayBoth v _) = atomByProp' p v
atomByProp _ v = atom v

type Deps = [(String, Property)] 

atomicExpression' :: Exp -> Property -> AbstractSto V -> Bool 
atomicExpression' e p sto = (atomByProp p $ abstractEval e sto) 

atomicExpression :: Exp -> Property -> AbstractSto V -> Bool 
-- | holds if the expression has an atomic value, either by direct computation or by case analysis of the coverings
atomicExpression e p sto = 
    let b = atomicExpression' e p sto
        stos = covering sto 
        b' = if (null stos) 
                then False 
                else and (map (atomicExpression' e p) $ covering sto)
    in b || b'

noDep' ::  Property -> Exp -> AbstractSto V -> Bool
noDep' p e sto = atomicExpression e p sto

noDep ::  Property -> (String, Property) -> Exp -> AbstractSto V -> Bool 
-- | holds if there is no dependency of e on x
noDep p (x, px) e sto = 
    let b = noDep' p e sto
        stos = xCoveringByProp x px sto
        b' = if (null stos)
                then False 
                else and (map (noDep' p e) stos)
    in b || b' 

findNDeps :: Exp -> (Maybe Property) -> AbstractSto V -> Deps 
findNDeps e (Just p) s = 
    let vars =  map (\a -> (a, PAll)) $ getVarsFromExp' e 
        nonDep = prove e s vars p 
    in vars \\ nonDep
findNDeps _ Nothing _ = error "no property given to find dependencies"

prove :: Exp -> AbstractSto V -> [(String, Property)] -> Property -> Deps 
prove e s xs p = evalState (prove' e s xs p) []

prove' :: Exp -> AbstractSto V -> [(String, Property)] -> Property -> State Deps Deps 
prove' e s xs p = 
    if and $ map (\x -> (noDep p x e s)) xs
        then do nonDep <- get 
                let nonDep' = union nonDep xs
                put nonDep'
                return nonDep' 
        else do mapM_ (\xs' -> prove' e s xs' p) [xs \\ [x] | x <- xs]
                nonDep <- get
                return nonDep