module Slicer(slice) where 

import Property.Agreement 
import GSystem 
import Syntax.Scheme.AST


slice :: Exp -> Criterion v -> Exp
-- | takes a program and a slicing criterion and returns the program where only statements affecting the criterion are remaining
-- TODO: do we really need two separate agreements? wouldn't just the final one be enough? (the one that belongs to the criterion)
slice p c = removeRedundantExp $ labelSequence p initialG finalG where (initialG, finalG) = criterionToAgreements c

removeRedundantExp :: LabeledExp -> Exp 
-- | makes use of property preservation to decide what statements to remove
removeRedundantExp = undefined
