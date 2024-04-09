{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Slicer where 

import Control.Monad.State    
import Prelude hiding (exp)
import Data.List (union)

import Property.Agreement 
import Property.Preservation
import Dependency.Lattice
import Dependency.State
import Labels 

import Syntax.Scheme.AST

slice :: Exp -> Agreement -> Exp
-- | takes a program and a criterion (= agreement (= list of relevant variables)) and returns the program where only statements affecting the criterion are remaining
slice p c = sliceExp p $ labelSequence p c

type UsedVars = [String]

type SliceState = State (AbstractSto V, UsedVars) Exp 

sliceUsed :: Exp -> Agreement -> (Exp, UsedVars)
sliceUsed p c = sliceExpUsed p $ labelSequence p c

sliceExpUsed :: Exp -> Labels -> (Exp, UsedVars)
sliceExpUsed e l = (e', vs) where (e', (_, vs)) = runState (sliceExp'' e l) (mempty, [])

-- | slices away all redundant expressions based on the given labels. 
sliceExp :: Exp -> Labels -> Exp 
sliceExp e l = evalState (sliceExp'' e l) (mempty, [])

sliceExp'' :: Exp -> Labels -> SliceState 
sliceExp'' e l = if isSkip l then return Empty else sliceExp' e l

sliceExp' :: Exp -> Labels -> SliceState 
sliceExp' e@(Let bds bdy s) l       = sliceLet e bds bdy s Let l
sliceExp' e@(Ltt bds bdy s) l       = sliceLet e bds bdy s Ltt l 
sliceExp' e@(Ltr bds bdy s) l       = sliceLet e bds bdy s Ltr l 
sliceExp' e@(Lrr bds bdy s) l       = sliceLet e bds bdy s Lrr l
sliceExp' e@(Dfv _ _ _) l           = sliceAssignment e l
sliceExp' e@(Set _ _ _) l           = sliceAssignment e l  
sliceExp' (Bgn es s) (Begin lbls)   = do es' <- mapM (uncurry sliceExp'') (zip es lbls)
                                         if null es' 
                                            then return Empty
                                            else return (Bgn es' s)    
sliceExp' e@(Iff _ _ _ _) l         = sliceIf e l                            

sliceAssignment :: Exp -> Labels -> SliceState 
sliceAssignment e (Binding g) = do  (sto, used) <- get
                                    let (b, sto') = preserveWithSto sto g e
                                    let used' = map name $ getVarsFromExp' e
                                    if b then do put (sto, used); return Empty 
                                         else do put (sto', union used used'); return e

sliceLet :: Exp -> [(Ide, Exp)] -> Exp -> Span -> ([(Ide, Exp)] -> Exp -> Span -> Exp) -> Labels -> SliceState
sliceLet e bds bdy s let' (Lett g lbls lbl) = do    (sto, used) <- get 
                                                    let (bds', (sto', used')) = runState (sliceBindings bds lbls) (sto, used)
                                                    put (sto', union used used')
                                                    bdy' <- sliceExp'' bdy lbl
                                                    if (preserve sto g e)  then do put (sto, used); return Empty 
                                                                           else return $ let' bds' bdy' s 

sliceBindings :: [(Ide, Exp)] -> [Labels] -> State ((AbstractSto V), UsedVars) [(Ide, Exp)]
sliceBindings bds lbls = do bds' <- mapM sliceBinding (zip bds lbls)
                            return $ concat bds'

sliceBinding :: ((Ide, Exp), Labels) -> State ((AbstractSto V), UsedVars) [(Ide, Exp)]
sliceBinding ((var, exp), (Binding g)) = do    (s, used) <- get
                                               let (b, s') = preserveWithSto s g (Set var exp NoSpan) 
                                               let used' = map name $ getVarsFromExp' exp
                                               if b then do put (s, used); return [] else do put (s', union used used'); return [(var, exp)]      

sliceIf :: Exp -> Labels -> SliceState 
sliceIf e@(Iff b c a s) (If g lblC lblA) = do   (sto, used) <- get
                                                c' <- sliceExp'' c lblC; (_, usedC) <- get; put (sto, union used usedC) 
                                                a' <- sliceExp'' a lblA; (_, usedA) <- get; put (sto, union usedC usedA)
                                                let used' = map name $ getVarsFromExp' b
                                                if (preserve sto g e) then do put (sto, used); return Empty else do put (sto, union usedA used'); return (Iff b c' a' s)