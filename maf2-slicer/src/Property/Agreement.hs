{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Property.Agreement(Agreement, agree, allStatesAgreeOn) where 

import Syntax.Scheme.AST
import Dependency.State
import Dependency.Lattice

import qualified Data.Map as Map
import Data.List.Extra

type Agreement = [Ide] -- a list of the variables that have to have the same abstract value 

agree :: (Eq v) => Agreement -> AbstractSto v -> AbstractSto v -> Bool
-- | two states agree <=> every property defined by the agreement is the same in both states
agree g s1 s2 = foldr (\k b -> b && (Map.lookup k s1) == (Map.lookup k s2)) True g

allStatesAgreeOn :: (RefinableLattice v) => Exp -> AbstractSto v -> Agreement -> Bool 
-- | allStatesAgreeOn is true <=> for all states that agree on the given agreement, the result of the expression is the same
allStatesAgreeOn e sto g = allSame $ map (abstractEval e) (foldr (\s l -> l ++ filter (agree g s) states) [] states) where states = (generateStates e sto)