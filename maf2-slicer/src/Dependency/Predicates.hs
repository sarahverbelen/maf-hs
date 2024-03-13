module Dependency.Predicates where 

--(atomicExpression :: Exp -> Sto -> Bool)
-- | holds if the expression e can be proved to have an atomic value U with state s, either by direct computation of the abstract semantics or by case-based reasoning    

-- (noDependency :: Exp -> Sto -> [Ide] -> Bool)
-- | holds if it can be proven that there is no dependency of e on variables in X. This can be proven
-- (1) directly, if e has an atomic value in the state (which implies that there is no dependency on variables in X or any other variables, or
-- (2) recursively, by replacing the state with an X-covering and trying to prove the result for every refinement.


-- atom-dep :: Exp -> Ide -> Agreement -> Agreement -> Bool 
