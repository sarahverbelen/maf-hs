{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Property.Agreement where 

import Syntax.Scheme.AST
import Dependency.State

import Data.Map as Map

-- (I, X, N)
-- initial states, relevant variables, program point
type Criterion v = ([AbstractSto Ide v], [Ide], [Int])

type Agreement = [Ide] -- agreement modeled as just a list of the variables that have to have the same abstract value 

-- initialAgreement: is an agreement where all variables are "abstracted" by their identity
initialAgreement :: Criterion v -> Agreement
initialAgreement = undefined

finalAgreement :: Criterion v -> Agreement
finalAgreement = undefined

criterionToAgreements :: Criterion v -> (Agreement, Agreement)
criterionToAgreements = undefined

-- two states agree <=> every property defined by the agreement is the same in both states
agree :: (Eq v, Ord var) => Map Ide var -> Map var v -> Map var v -> Agreement-> Bool -- TODO: rewrite for abstract state
agree env s1 s2 g = Map.foldr (\k b -> b && (Map.lookup k s1) == (Map.lookup k s2)) True $ Map.filterWithKey (\k _ -> k `elem` g) env