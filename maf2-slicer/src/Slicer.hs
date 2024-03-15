module Slicer where 

import Property.Agreement 
import GSystem 
import Syntax.Scheme.AST


slice :: Exp -> Criterion v -> Exp
---- | takes a program and a slicing criterion and returns the program where only statements affecting the criterion are remaining
slice p c = removeRedundantExp $ labelSequence p initialG finalG where (initialG, finalG) = criterionToAgreements c

removeRedundantExp :: LabeledExp -> Exp 
removeRedundantExp = undefined
-- makes use o property preservation to decide what statements to remove