{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Slicer(slice) where 

import Property.Agreement 
import GSystem 
import Syntax.Scheme.AST


slice :: forall v . (Eq v) => Exp -> Agreement -> Exp
-- (v is the type of the abstract values (aka the domain we are working in))
-- | takes a program and a criterion (= agreement) and returns the program where only statements affecting the criterion are remaining
slice p c = removeRedundantExp $ labelSequence @v p c

removeRedundantExp :: Labels -> Exp 
-- | makes use of property preservation to decide what statements to remove
removeRedundantExp = undefined
