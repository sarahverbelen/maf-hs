{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Property.Agreement(Agreement, agree) where 

import Syntax.Scheme.AST
import Dependency.State

import Data.Map as Map

type Agreement = [Ide] -- agreement modeled as just a list of the variables that have to have the same abstract value 

-- two states agree <=> every property defined by the agreement is the same in both states
agree :: (Eq v, Ord var) => Map Ide var -> Map var v -> Map var v -> Agreement-> Bool -- TODO: rewrite for abstract state
agree env s1 s2 g = Map.foldr (\k b -> b && (Map.lookup k s1) == (Map.lookup k s2)) True $ Map.filterWithKey (\k _ -> k `elem` g) env