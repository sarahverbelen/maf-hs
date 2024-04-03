module Dependency.Dependency(dependencies) where 

import Dependency.Lattice 
import Dependency.State

import Data.List ((\\))

import Syntax.Scheme.AST

atomicExpression :: Exp -> AbstractSto V -> Bool 
-- | holds if the expression has an atomic value, either by direct computation or by case analysis of the coverings
atomicExpression e sto = (atom $ abstractEval e sto) || and (map (atomicExpression e) $ covering (extendStateForExp e sto))

noDep ::  Ide -> Exp -> AbstractSto V -> Bool 
-- | holds if there is no dependency of e on x
noDep x e sto = (atomicExpression e sto) || and (map (noDep x e) $ xCovering [x] (extendStateForExp e sto)) 

dependencies :: Exp -> AbstractSto V -> [String] 
-- | returns a list of all variables the expression is dependent on (based on the findNDep algorithm)
dependencies e sto = map name $ if atomicExpression e sto then vars else vars \\ noDeps 
    where   vars = (getVarsFromExp' e)
            noDeps = filter (\x -> noDep x e sto) vars
