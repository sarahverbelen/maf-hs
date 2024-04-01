{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Slicer(slice) where 

import Property.Agreement 
import Property.Preservation
import Dependency.Lattice
import GSystem 
import Syntax.Scheme.AST

slice :: Exp -> Agreement -> Exp
-- (v is the type of the abstract values (aka the domain we are working in))
-- | takes a program and a criterion (= agreement (= list of relevant variables)) and returns the program where only statements affecting the criterion are remaining
slice p c = removeRedundantExp p $ labelSequence p c

removeRedundantExp :: Exp -> Labels -> Exp 
-- | makes use of the labels to decide what expressions to remove
-- TODO (keep track of AbstractSto for preserve)
removeRedundantExp _ (Skip _) = Empty 
removeRedundantExp e@(Dfv _ _ _) (Binding g) = if (preserve mempty g e) then Empty else e
removeRedundantExp e@(Set _ _ _) (Binding g) = if (preserve mempty g e) then Empty else e
removeRedundantExp (Bgn es s) (Begin _ lbls) = (Bgn (map (uncurry (removeRedundantExp)) (zip es lbls)) s) 
removeRedundantExp e@(Iff b c a s) (If g lblC lblA) = if (preserve mempty g e) then Empty else Iff b (removeRedundantExp c lblC) (removeRedundantExp a lblA) s 
removeRedundantExp e@(Let bds bdy s) lbl = removeRedundantLet e bds bdy s Let lbl      
removeRedundantExp e@(Ltt bds bdy s) lbl = removeRedundantLet e bds bdy s Ltt lbl     
removeRedundantExp e@(Ltr bds bdy s) lbl = removeRedundantLet e bds bdy s Ltr lbl     
removeRedundantExp e@(Lrr bds bdy s) lbl = removeRedundantLet e bds bdy s Lrr lbl     
removeRedundantExp e _ = e 

removeRedundantLet :: Exp -> [(Ide, Exp)] -> Exp -> Span -> ([(Ide, Exp)] -> Exp -> Span -> Exp) -> Labels -> Exp 
-- TODO: map over bindings
removeRedundantLet e bds bdy s let' (Lett g lbls lbl) = if (preserve mempty g e) then Empty else let' bds (removeRedundantExp bdy lbl) s
removeRedundantLet _ _ _ _ _ _ = error "illegal label for let"