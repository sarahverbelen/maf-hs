module GSystem where 

import Property.Agreement 
import Syntax.Scheme.AST

type LabeledExp = Exp

labelSequence :: Exp -> Agreement -> Agreement -> LabeledExp
labelSequence = undefined

-- labelStatement :: Exp -> Agreement -> Agreement
-- | implements the rules as described in the G-system