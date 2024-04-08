module Dependency.Dependency where 

import Dependency.Lattice 
import Dependency.State
import Property.Agreement

import Data.List ((\\))

import Syntax.Scheme.AST
import Domain.Scheme hiding (Exp)

atomByProp :: Property -> V-> Bool 
atomByProp PAll v = atom v
atomByProp PReal v = atom $ real v
atomByProp PInt v = atom $ integer v 
atomByProp PBool v = atom $ boolean v

atomicExpression :: Exp -> Property -> AbstractSto V -> Bool 
-- | holds if the expression has an atomic value, either by direct computation or by case analysis of the coverings
atomicExpression e p sto = (atomByProp p $ abstractEval e sto) || and (map (atomicExpression e p) $ covering sto) -- TODO: this probably creates a loop because of the covering..

noDep ::  Property -> (Ide, Property) -> Exp -> AbstractSto V -> Bool 
-- | holds if there is no dependency of e on x
noDep p (x, px) e sto = (atomicExpression e p sto) || and (map (noDep p (x, px) e) $ xCoveringByProp x px sto) -- TODO: this probably creates a loop because of the covering..

dependencies :: Exp -> Maybe Property -> AbstractSto V -> [(String, Property)] 
-- | returns a list of all variables the expression is dependent on for the given property (based on the findNDep algorithm)
dependencies e (Just p) sto = map (\(a, b) -> (name a, b)) $ if atomicExpression e p sto then vars else vars \\ noDeps 
    where   vars = map (\x -> (x, PInt)) (getVarsFromExp' e)
            noDeps = filter (\x -> noDep p x e (extendStateForExp e sto)) vars
dependencies _ Nothing _ = error "no property given to find dependencies"
