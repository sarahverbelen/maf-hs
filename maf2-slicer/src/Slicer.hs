{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Slicer(slice) where 

import Property.Agreement 
import GSystem 
import Syntax.Scheme.AST



slice :: forall v . (Eq v) => Exp -> Agreement -> Exp
-- (v is the type of the abstract values (aka the domain we are working in))
-- | takes a program and a criterion (= agreement ()= list of relevant variables)) and returns the program where only statements affecting the criterion are remaining
slice p c = removeRedundantExp p $ labelSequence @v p c

removeRedundantExp :: Exp -> Labels -> Exp 
-- | makes use of the labels and property preservation to decide what statements to remove
removeRedundantExp = undefined
