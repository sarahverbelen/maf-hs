module Slicer where 

-- (I, X, N, A)
-- initial states, relevant variables, program point, initial agreement
-- type Criterion = ([State], [Ide], [Int], Agreement)

-- slice :: Exp -> Criterion -> Exp
---- | takes a program and a slicing criterion and returns the program where only statements affecting the criterion are remaining
--slice p (_ _ _ finalG) = removeRedundantExp $ labelSequence p initialG finalG (const True) 

--removeRedundantExp :: LabeledExp -> Exp 
-- makes use o property preservation to decide what statements to remove