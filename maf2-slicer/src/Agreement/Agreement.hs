{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Agreement.Agreement where 

import Analysis.Scheme.Monad
import Analysis.Scheme
import Domain
import Syntax.Scheme.AST
import Interpreter.Scheme

import Data.Map

data Property v = BoolProp v | SignProp v |IdProp v

-- an agreement is a set of constraints: variables for which they have to agree on the property
type Agreement v = Map CAdr (Property v)

-- initialAgreement: is an agreement where all variables are "abstracted" by their identity

-- two states agree <=> every property defined by the agreement is the same in both states
agree :: (SchemeDomain v) => DSto ctx v -> DSto ctx v -> Agreement v -> Bool 
agree s1 s2 g = True