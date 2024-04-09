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

sliceExp :: Exp -> Labels -> Exp 
sliceExp e l = sliceExp' e toslice vars where (toslice, vars) = labelIrrelevant e l

sliceExp' :: Exp -> ToSlice -> UsedVars -> Exp 
sliceExp' (Let bds bdy s) toslice vars = sliceLet bds bdy s Let toslice vars 
sliceExp' (Ltt bds bdy s) toslice vars = sliceLet bds bdy s Ltt toslice vars 
sliceExp' (Ltr bds bdy s) toslice vars = sliceLet bds bdy s Ltr toslice vars 
sliceExp' (Lrr bds bdy s) toslice vars = sliceLet bds bdy s Lrr toslice vars 
sliceExp' (Dfv var e s) toslice vars   = sliceAssignment var e s Dfv toslice vars
sliceExp' (Set var e s) toslice vars   = sliceAssignment var e s Set toslice vars
sliceExp' (Bgn es s) toslice vars      = sliceBegin es s toslice vars
sliceExp' (Iff b c a s) toslice vars   = sliceIf b c a s toslice vars 
sliceExp' e (Lbl False) _              = e 
sliceExp' _ _ _                        = Empty


sliceLet :: [(Ide, Exp)] -> Exp -> Span -> ([(Ide, Exp)] -> Exp -> Span -> Exp) -> ToSlice -> UsedVars -> Exp 
sliceLet bds bdy s let' (Lbl True) vars = Empty 
sliceLet bds bdy s let' (Lbls ((Lbl False):lblBds:lblBdy)) vars = let' bds' bdy' s where bds' = sliceBinds bds lblBds vars
                                                                                         bdy' = sliceExp' bdy (head lblBdy) vars

sliceBinds :: [(Ide, Exp)] -> ToSlice -> UsedVars -> [(Ide, Exp)] 
sliceBinds bds (Lbls toslice) vars = map fst $ filter (\(bd, lbl) -> sliceBind bd lbl vars) (zip bds toslice)

sliceBind :: (Ide, Exp) -> ToSlice -> UsedVars -> Bool 
sliceBind bd (Lbl False) vars = True 
sliceBind (var, e) (Lbl True) vars = if (name var) `elem` vars then True else False

sliceAssignment :: Ide -> Exp -> Span -> (Ide -> Exp -> Span -> Exp) -> ToSlice -> UsedVars -> Exp 
sliceAssignment var e s def' (Lbl False) vars = def' var e s
sliceAssignment var e s def' (Lbl True) vars  = if (name var) `elem` vars then (def' var e s) else Empty

sliceBegin :: [Exp] -> Span -> ToSlice -> UsedVars -> Exp 
sliceBegin es s (Lbl True) vars = Empty 
sliceBegin es s (Lbls ((Lbl False):lbls)) vars = let es' = map (\(e, l) -> sliceExp' e l vars) (zip es lbls) in (Bgn es' s)

sliceIf :: Exp -> Exp -> Exp -> Span -> ToSlice -> UsedVars -> Exp 
sliceIf b c a s (Lbl True) vars = Empty 
sliceIf b c a s (Lbls ((Lbl False):lblC:lblA)) vars = Iff b c' a' s where c' = sliceExp' c lblC vars
                                                                          a' = sliceExp' a (head lblA) vars 

data ToSlice = Lbls [ToSlice] | Lbl Bool deriving (Show, Eq)
type LabelIrrState = State (AbstractSto V, UsedVars) ToSlice


labelIrrelevant :: Exp -> Labels -> (ToSlice, UsedVars)
labelIrrelevant e l = (e', vs) where (e', (_, vs)) = runState (labelIrrelevant' e l) (mempty, [])

labelIrrelevant' :: Exp -> Labels -> LabelIrrState 
labelIrrelevant' e l = if isSkip l then return (Lbl True) else labelIrrExp' e l

labelIrrExp' :: Exp -> Labels -> LabelIrrState 
labelIrrExp' e@(Let bds bdy s) l       = labelIrrLet e bds bdy s Let l
labelIrrExp' e@(Ltt bds bdy s) l       = labelIrrLet e bds bdy s Ltt l 
labelIrrExp' e@(Ltr bds bdy s) l       = labelIrrLet e bds bdy s Ltr l 
labelIrrExp' e@(Lrr bds bdy s) l       = labelIrrLet e bds bdy s Lrr l
labelIrrExp' e@(Dfv _ _ _) l           = labelIrrAssignment e l
labelIrrExp' e@(Set _ _ _) l           = labelIrrAssignment e l  
labelIrrExp' (Bgn es s) (Begin lbls)   = do es' <- mapM (uncurry labelIrrelevant') (zip es lbls)
                                            if null es' 
                                               then return $ Lbl True
                                               else return $ Lbls ([Lbl False] ++ es') 
labelIrrExp' e@(Iff _ _ _ _) l         = labelIrrIf e l                            

labelIrrAssignment :: Exp -> Labels -> LabelIrrState 
labelIrrAssignment e (Binding g) = do  (sto, used) <- get
                                       let (b, sto') = preserveWithSto sto g e
                                       let used' = map name $ getVarsFromExp' e
                                       if b then do put (sto, used); return $ Lbl True
                                            else do put (sto', union used used'); return $ Lbl False

labelIrrLet :: Exp -> [(Ide, Exp)] -> Exp -> Span -> ([(Ide, Exp)] -> Exp -> Span -> Exp) -> Labels -> LabelIrrState
labelIrrLet e bds bdy s let' (Lett g lbls lbl) = do (sto, used) <- get 
                                                    let (bds', (sto', used')) = runState (labelIrrBindings bds lbls) (sto, used)
                                                    put (sto', union used used')
                                                    bdy' <- labelIrrelevant' bdy lbl
                                                    if (preserve sto g e)  then do put (sto, used); return $ Lbl True 
                                                                           else return $ Lbls ([Lbl False] ++  [bds'] ++ [bdy'])

labelIrrBindings :: [(Ide, Exp)] -> [Labels] -> LabelIrrState
labelIrrBindings bds lbls = do bds' <- mapM labelIrrBinding (zip bds lbls)
                               return $ Lbls bds'

labelIrrBinding :: ((Ide, Exp), Labels) -> LabelIrrState
labelIrrBinding ((var, exp), (Binding g)) = do (s, used) <- get
                                               let (b, s') = preserveWithSto s g (Set var exp NoSpan) 
                                               let used' = map name $ getVarsFromExp' exp
                                               if b then do put (s, used); return (Lbl True) else do put (s', union used used'); return (Lbl False)      

labelIrrIf :: Exp -> Labels -> LabelIrrState 
labelIrrIf e@(Iff b c a s) (If g lblC lblA) = do   (sto, used) <- get
                                                   c' <- labelIrrelevant' c lblC; (_, usedC) <- get; put (sto, union used usedC) 
                                                   a' <- labelIrrelevant' a lblA; (_, usedA) <- get; put (sto, union usedC usedA)
                                                   let used' = map name $ getVarsFromExp' b
                                                   if (preserve sto g e) then do put (sto, used); return (Lbl True) else do put (sto, union usedA used'); return $ Lbls ([Lbl False] ++ [c'] ++ [a'])