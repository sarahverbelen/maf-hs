module Dependency.Dependency where 

import Dependency.Lattice 
import Dependency.State
import Property.Agreement

import Data.List ((\\))

import Syntax.Scheme.AST
import Domain.Scheme hiding (Exp)
import qualified Control.Monad.Escape as Escape

atomByProp' :: Property -> V -> Bool 
atomByProp' PAll  v = atom v
atomByProp' PReal v = atom $ real v
atomByProp' PInt  v = atom $ integer v 
atomByProp' PBool v = atom $ boolean v

atomByProp :: Property -> Value -> Bool 
atomByProp p (Escape.Value v) = atomByProp' p v
atomByProp p (Escape.MayBoth v _) = atomByProp' p v
atomByProp _ v = atom v

-- possibly infinitely looping versions
atomicExpression :: Exp -> Property -> AbstractSto V -> Bool 
-- | holds if the expression has an atomic value, either by direct computation or by case analysis of the coverings
atomicExpression e p sto = (atomByProp p $ abstractEval e sto) || and (map (atomicExpression e p) $ covering sto)

noDep ::  Property -> (Ide, Property) -> Exp -> AbstractSto V -> Bool 
-- | holds if there is no dependency of e on x
noDep p (x, px) e sto = (atomicExpression e p sto) || and (map (noDep p (x, px) e) $ xCoveringByProp x px sto)

-- versions limited by iteration count
iterationCount :: Int 
iterationCount = 16

atomicExpression' :: Int -> Exp -> Property -> AbstractSto V -> Bool 
-- | holds if the expression has an atomic value, either by direct computation or by case analysis of the coverings
atomicExpression' 0 e p sto = (atomByProp p $ abstractEval e sto)
atomicExpression' n e p sto = (atomByProp p $ abstractEval e sto) || and (map (atomicExpression' (n - 1) e p) $ covering sto)

noDep' ::  Int -> Property -> (Ide, Property) -> Exp -> AbstractSto V -> Bool 
-- | holds if there is no dependency of e on x
noDep' 0 p (x, px) e sto = (atomicExpression' iterationCount e p sto) 
noDep' n p (x, px) e sto = (atomicExpression' n e p sto) || and (map (noDep' (n - 1) p (x, px) e) $ xCoveringByProp x px sto)

dependencies :: Exp -> Maybe Property -> AbstractSto V -> [(String, Property)] 
-- | returns a list of all variables the expression is dependent on for the given property (based on the findNDep algorithm)
dependencies e (Just p) sto = map (\(a, b) -> (name a, b)) $ if atomicExpression' iterationCount e p sto then vars else  noDeps 
    where   vars = map (\x -> (x, PAll)) (getVarsFromExp' e) -- to improve: figure out what property is a dependency
            noDeps = filter (\x -> noDep' iterationCount p x e (extendStateForExp e sto)) vars
dependencies _ Nothing _ = error "no property given to find dependencies"
