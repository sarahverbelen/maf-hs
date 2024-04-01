{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Slicer(slice) where 

import Control.Monad.State    
import Prelude hiding (exp)

import Property.Agreement 
import Property.Preservation
import Dependency.Lattice
import Dependency.State
import GSystem 

import Syntax.Scheme.AST

slice :: Exp -> Agreement -> Exp
-- | takes a program and a criterion (= agreement (= list of relevant variables)) and returns the program where only statements affecting the criterion are remaining
slice p c = sliceExp p $ labelSequence p c

type SliceState = State (AbstractSto V, Labels) Exp 

sliceExp :: Exp -> Labels -> Exp 
sliceExp e lbls = evalState (sliceExp' e) (mempty, lbls) 

sliceExp' :: Exp -> SliceState 
sliceExp' e@(Let bds bdy s) = sliceLet e bds bdy s Let 
sliceExp' e@(Ltt bds bdy s) = sliceLet e bds bdy s Ltt 
sliceExp' e@(Ltr bds bdy s) = sliceLet e bds bdy s Ltr 
sliceExp' e@(Lrr bds bdy s) = sliceLet e bds bdy s Lrr
sliceExp' e@(Dfv _ _ _)     = sliceAssignment e
sliceExp' e@(Set _ _ _)     = sliceAssignment e   
sliceExp' (Bgn es s)        = do    (_, lbl) <- get -- possible improvement: remove whole begin if it preserves its label
                                    let (Begin _ lbls) = lbl
                                    return (Bgn (map (uncurry (removeRedundantExp)) (zip es lbls)) s)                                
sliceExp' e                 = do    (_, lbls) <- get
                                    if isSkip lbls then return Empty else return e


sliceAssignment :: Exp -> SliceState 
sliceAssignment e = do  (sto, lbl) <- get
                        let (Binding g) = lbl
                        if (preserve sto g e) then return Empty else return e

sliceLet :: Exp -> [(Ide, Exp)] -> Exp -> Span -> ([(Ide, Exp)] -> Exp -> Span -> Exp) -> SliceState
sliceLet e bds bdy s let' = do  (_, l) <- get 
                                let (Lett g lbls lbl) = l
                                let bdy' = (removeRedundantExp bdy lbl)
                                let bds' = map fst $ filter (\((var, exp), (Binding g')) -> not $ preserve mempty g' (Set var exp NoSpan)) $ zip bds lbls
                                if (preserve mempty g e) then return Empty else return $ let' bds' bdy' s 