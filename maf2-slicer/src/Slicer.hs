{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Slicer where 

import Control.Monad.State    
import Prelude hiding (exp)
import Data.List (union, (\\))

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

type SliceState = State UsedVars Exp 

sliceExp :: Exp -> Labels -> Exp 
sliceExp e l = evalState (sliceExp' e toslice) vars where (toslice, vars) = labelIrrelevant e l

sliceExp' :: Exp -> ToSlice -> SliceState 
sliceExp' (Let bds bdy s) toslice = sliceLet bds bdy s Let toslice 
sliceExp' (Ltt bds bdy s) toslice = sliceLet bds bdy s Ltt toslice 
sliceExp' (Ltr bds bdy s) toslice = sliceLet bds bdy s Ltr toslice 
sliceExp' (Lrr bds bdy s) toslice = sliceLet bds bdy s Lrr toslice 
sliceExp' (Dfv var e s) toslice   = sliceAssignment var e s Dfv toslice
sliceExp' (Set var e s) toslice   = sliceAssignment var e s Set toslice
sliceExp' (Bgn es s) toslice      = sliceBegin es s toslice
sliceExp' (Iff b c a s) toslice   = sliceIf b c a s toslice 
sliceExp' e (Lbl False)           = return e 
sliceExp' _ _                     = return Empty


sliceLet :: [(Ide, Exp)] -> Exp -> Span -> ([(Ide, Exp)] -> Exp -> Span -> Exp) -> ToSlice -> SliceState  
sliceLet bds bdy s let' (Lbl True) = return Empty 
sliceLet bds bdy s let' (Lbls ((Lbl False):lblBds:lblBdy)) = do 
   vars <- get
   let (bds', vars') = runState (sliceBinds bds lblBds) vars
   bdy' <- sliceExp' bdy (head lblBdy)
   return $ let' bds' bdy' s                                                                                        

sliceBinds :: [(Ide, Exp)] -> ToSlice -> State UsedVars [(Ide, Exp)] 
sliceBinds [] _ = return []
sliceBinds (bd:bds) (Lbls (l:toslice)) = do 
   vars <- get
   let b = sliceBind bd l vars 
   let (var, _) = bd
   if b 
      then do nextBds <- sliceBinds bds (Lbls toslice); return $ nextBds
      else do put (vars \\ [name var]); nextBds <- sliceBinds bds (Lbls toslice); return $ [bd] ++ nextBds

sliceBind :: (Ide, Exp) -> ToSlice -> UsedVars -> Bool 
sliceBind bd (Lbl False) vars = False 
sliceBind (var, e) (Lbl True) vars = if (name var) `elem` vars then False else True

sliceAssignment :: Ide -> Exp -> Span -> (Ide -> Exp -> Span -> Exp) -> ToSlice -> SliceState 
sliceAssignment var e s def' (Lbl False) = return $ def' var e s
sliceAssignment var e s def' (Lbl True)  = do 
   vars <- get
   if (name var) `elem` vars 
      then do put (vars \\ [name var]); return $ def' var e s
      else return Empty

sliceBegin :: [Exp] -> Span -> ToSlice -> SliceState  
sliceBegin es s (Lbl True) = return Empty 
sliceBegin es s (Lbls ((Lbl False):lbls)) = do
   vars <- get
   es' <- mapM (\(e, l) -> sliceExp' e l) (zip es lbls)
   return $ Bgn es' s

sliceIf :: Exp -> Exp -> Exp -> Span -> ToSlice -> SliceState
sliceIf b c a s (Lbl True) = return Empty 
sliceIf b c a s (Lbls ((Lbl False):lblC:lblA)) = do 
   c' <- sliceExp' c lblC 
   a' <- sliceExp' a (head lblA)
   return $ Iff b c' a' s

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