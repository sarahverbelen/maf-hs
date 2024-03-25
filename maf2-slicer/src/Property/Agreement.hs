{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Property.Agreement(Agreement, agree, allStatesAgreeOn) where 

import Syntax.Scheme.AST
import Dependency.State

import qualified Data.Map as Map

type Agreement = [Ide] -- agreement modeled as just a list of the variables that have to have the same abstract value 


-- two states agree <=> every property defined by the agreement is the same in both states
agree :: (Eq v) => AbstractSto v -> AbstractSto v -> Agreement -> Bool
agree s1 s2 g = foldr (\k b -> b && (Map.lookup k s1) == (Map.lookup k s2)) True g


-- | TODO
-- allStatesAgreeOn is true <=> for all states that agree on the given agreement, the result of the expression is the same
allStatesAgreeOn :: Exp -> AbstractSto v -> Agreement -> Bool 
allStatesAgreeOn e sto g = True