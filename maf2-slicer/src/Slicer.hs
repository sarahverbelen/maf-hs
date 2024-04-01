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
sliceExp e lbls = evalState (sliceExp'' e) (mempty, lbls) 

sliceExp'' :: Exp -> SliceState 
sliceExp'' e = do (_, lbls) <- get 
                  if isSkip lbls then return Empty else sliceExp' e

sliceExp' :: Exp -> SliceState 
sliceExp' e@(Let bds bdy s) = sliceLet e bds bdy s Let 
sliceExp' e@(Ltt bds bdy s) = sliceLet e bds bdy s Ltt 
sliceExp' e@(Ltr bds bdy s) = sliceLet e bds bdy s Ltr 
sliceExp' e@(Lrr bds bdy s) = sliceLet e bds bdy s Lrr
sliceExp' e@(Dfv _ _ _)     = sliceAssignment e
sliceExp' e@(Set _ _ _)     = sliceAssignment e   
sliceExp' (Bgn es s)        = do    (_, lbl) <- get -- possible improvement: remove whole begin if it preserves its label
                                    let (Begin l lbls) = lbl
                                    return (Bgn (map (uncurry (sliceExp)) (zip es lbls)) s)                                


sliceAssignment :: Exp -> SliceState 
sliceAssignment e = do  (sto, lbl) <- get
                        let (Binding g) = lbl
                        let (b, sto') = preserveWithSto sto g e
                        put (sto', lbl)
                        if b then return Empty else return e

sliceLet :: Exp -> [(Ide, Exp)] -> Exp -> Span -> ([(Ide, Exp)] -> Exp -> Span -> Exp) -> SliceState
sliceLet e bds bdy s let' = do  (sto, l) <- get 
                                let (Lett g lbls lbl) = l
                                let bdy' = (sliceExp bdy lbl)
                                let (bds', sto') = runState (sliceBindings bds lbls) sto
                                put (sto', l)
                                if (preserve mempty g e) then return Empty else return $ let' bds' bdy' s 

type BindingState = State (AbstractSto V) [(Ide, Exp)]

sliceBindings :: [(Ide, Exp)] -> [Labels] -> BindingState
sliceBindings bds lbls = do bds' <- mapM sliceBinding (zip bds lbls)
                            return $ concat bds'


sliceBinding :: ((Ide, Exp), Labels) -> BindingState 
sliceBinding ((var, exp), (Binding g)) = do    s <- get
                                               let (b, s') = preserveWithSto s g (Set var exp NoSpan) 
                                               put s'
                                               if b then return [] else return [(var, exp)]      