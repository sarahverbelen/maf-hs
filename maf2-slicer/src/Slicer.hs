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
import Labels 

import Syntax.Scheme.AST

slice :: Exp -> Agreement -> Exp
-- | takes a program and a criterion (= agreement (= list of relevant variables)) and returns the program where only statements affecting the criterion are remaining
slice p c = sliceExp p $ labelSequence p c

type SliceState = State (AbstractSto V) Exp 


-- | slices away all redundant expressions based on the given labels. 
sliceExp :: Exp -> Labels -> Exp 
sliceExp e l = evalState (sliceExp'' e l) mempty 

sliceExp'' :: Exp -> Labels -> SliceState 
sliceExp'' e l = if isSkip l then return Empty else sliceExp' e l

sliceExp' :: Exp -> Labels -> SliceState 
sliceExp' e@(Let bds bdy s) l       = sliceLet e bds bdy s Let l
sliceExp' e@(Ltt bds bdy s) l       = sliceLet e bds bdy s Ltt l 
sliceExp' e@(Ltr bds bdy s) l       = sliceLet e bds bdy s Ltr l 
sliceExp' e@(Lrr bds bdy s) l       = sliceLet e bds bdy s Lrr l
sliceExp' e@(Dfv _ _ _) l           = sliceAssignment e l
sliceExp' e@(Set _ _ _) l           = sliceAssignment e l  
sliceExp' (Bgn es s) (Begin l lbls) = return (Bgn (map (uncurry (sliceExp)) (zip es lbls)) s)                                

sliceAssignment :: Exp -> Labels -> SliceState 
sliceAssignment e (Binding g) = do  sto <- get
                                    let (b, sto') = preserveWithSto sto g e
                                    put sto'
                                    if b then return Empty else return e

-- TODO: doesn't seem to work right :(
sliceLet :: Exp -> [(Ide, Exp)] -> Exp -> Span -> ([(Ide, Exp)] -> Exp -> Span -> Exp) -> Labels -> SliceState
sliceLet e bds bdy s let' (Lett g lbls lbl) = do    sto <- get 
                                                    let bdy' = (sliceExp bdy lbl)
                                                    let (bds', sto') = runState (sliceBindings bds lbls) sto
                                                    put sto'
                                                    if (preserve mempty g e) then return Empty else return $ let' bds' bdy' s 

sliceBindings :: [(Ide, Exp)] -> [Labels] -> State (AbstractSto V) [(Ide, Exp)]
sliceBindings bds lbls = do bds' <- mapM sliceBinding (zip bds lbls)
                            return $ concat bds'

sliceBinding :: ((Ide, Exp), Labels) -> State (AbstractSto V) [(Ide, Exp)]
sliceBinding ((var, exp), (Binding g)) = do    s <- get
                                               let (b, s') = preserveWithSto s g (Set var exp NoSpan) 
                                               put s'
                                               if b then return [] else return [(var, exp)]      