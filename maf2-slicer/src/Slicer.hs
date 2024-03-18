module Slicer(slice) where 

import Property.Agreement 
import GSystem 
import Syntax.Scheme.AST


slice :: Exp -> Agreement -> Exp
-- | takes a program and a criterion (= agreement) and returns the program where only statements affecting the criterion are remaining
slice p c = removeRedundantExp $ labelSequence p c

removeRedundantExp :: LabeledExp -> Exp 
-- | makes use of property preservation to decide what statements to remove
removeRedundantExp = undefined
