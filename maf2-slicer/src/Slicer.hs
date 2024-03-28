{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Slicer(slice) where 

import Property.Agreement 
import Property.Preservation
import Dependency.Lattice
import GSystem 
import Syntax.Scheme.AST

slice :: forall v . (RefinableLattice v) => Exp -> Agreement -> Exp
-- (v is the type of the abstract values (aka the domain we are working in))
-- | takes a program and a criterion (= agreement (= list of relevant variables)) and returns the program where only statements affecting the criterion are remaining
slice p c = removeRedundantExp @v p $ labelSequence @v p c

-- TODO: keep track of AbstractSto to use in preserve
removeRedundantExp :: forall v . (RefinableLattice v) => Exp -> Labels -> Exp 
-- | makes use of the labels to decide what expressions to remove
-- an expression can be removed if it preserves the properties in its label
-- removeRedundantExp e@(Dfv _ _ _) gs = if preserve @v mempty (head gs) e then Empty else e 
-- removeRedundantExp e@(Set _ _ _) gs = if preserve @v mempty (head gs) e then Empty else e 
-- removeRedundantExp e@(Iff b c a s) (g:gs) = if preserve @v mempty g e then Empty else (Iff b (removeRedundantExp @v c gs) (removeRedundantExp @v a gs) s) 

removeRedundantExp e _ = e 
