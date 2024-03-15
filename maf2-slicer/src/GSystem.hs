module GSystem where 

import Property.Agreement 
import Syntax.Scheme.AST

type LabeledExp = Exp

-- label all statements in the sequence with agreements by backwards propagating the G-system rules
labelSequence :: Exp -> Agreement -> Agreement -> LabeledExp
labelSequence e initialG finalG = undefined

-- labelStatement :: Exp -> Agreement -> Agreement
-- | implements the rules as described in the G-system