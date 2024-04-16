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

type UsedVars = [String] -- TODO: rework so its also nested (follows the structure of the expressions) (is a used variable analysis!)
-- data ToSlice = Lbls [ToSlice] | Lbl Bool deriving (Show, Eq)
data ToSlice = LetS Bool [ToSlice] ToSlice | IfS Bool ToSlice ToSlice | BindingS Bool ToSlice | SkipS Bool | BeginS Bool [ToSlice] deriving (Eq, Show)

sliceBool :: ToSlice -> Bool 
sliceBool (LetS b _ _)   = b
sliceBool (IfS b _ _)    = b 
sliceBool (BindingS b _) = b 
sliceBool (SkipS b)      = b 
sliceBool (BeginS b _)   = b

type SliceState = State UsedVars Exp 

dummyExp :: Span -> Exp 
dummyExp s = Var (Ide "dummy" s)

sliceExp :: Exp -> Labels -> Exp 
sliceExp e l = evalState (sliceExp' e toslice) vars where (toslice, vars) = labelIrrelevant e l

sliceExp' :: Exp -> ToSlice -> SliceState 
sliceExp' e (SkipS True)            = return (Nll (spanOf e))
sliceExp' (Let bds bdy s) toslice = sliceLet bds bdy s Let toslice 
sliceExp' (Ltt bds bdy s) toslice = sliceLet bds bdy s Ltt toslice 
sliceExp' (Ltr bds bdy s) toslice = sliceLet bds bdy s Ltr toslice 
sliceExp' (Lrr bds bdy s) toslice = sliceLet bds bdy s Lrr toslice 
sliceExp' (Dfv var e s) toslice   = sliceAssignment var e s False toslice
sliceExp' (Set var e s) toslice   = sliceAssignment var e s True toslice
sliceExp' (Bgn es s) toslice      = sliceBegin es s toslice
sliceExp' (Iff b c a s) toslice   = sliceIf b c a s toslice  
sliceExp' e _                     = return e


sliceLet :: [(Ide, Exp)] -> Exp -> Span -> ([(Ide, Exp)] -> Exp -> Span -> Exp) -> ToSlice -> SliceState  
sliceLet bds bdy s let' (LetS True _ _) = return (Nll s) 
sliceLet bds bdy s let' (LetS False lblBds lblBdy) = do 
   vars <- get
   let (bds', vars') = runState (sliceBinds bds lblBds) vars
   bdy' <- sliceExp' bdy lblBdy
   return $ let' bds' bdy' s                                                                                        

sliceBinds :: [(Ide, Exp)] -> [ToSlice] -> State UsedVars [(Ide, Exp)] 
sliceBinds [] _ = return []
sliceBinds (bd:bds) (l@(BindingS _ eLbl):toslice) = do 
   vars <- get
   let b = sliceBind bd l vars 
   let (var, e) = bd
   -- e' <- sliceExp' e eLbl
   if (b == (Just True)) 
      then do nextBds <- sliceBinds bds toslice; return $ nextBds -- slice the binding completely
      else if (b == Nothing) 
         then do put (vars \\ [name var]); nextBds <- sliceBinds bds toslice; return $ [(var, e)] ++ nextBds -- keep the binding
         else do put (vars \\ [name var]); nextBds <- sliceBinds bds toslice; return $ [(var, dummyExp (spanOf e))] ++ nextBds -- dummify the binding

sliceBind :: (Ide, Exp) -> ToSlice -> UsedVars -> Maybe Bool 
sliceBind bd (BindingS False _) vars = Nothing -- this binding is necessary
sliceBind (var, e) (BindingS True _) vars = if (name var) `elem` vars 
   then Just False -- this binding could be sliced away but it is used in some other expression so we need to dummify it
   else Just True -- this binding can be sliced away

sliceAssignment :: Ide -> Exp -> Span -> Bool -> ToSlice -> SliceState 
sliceAssignment var e s set (BindingS False eLbl) = do 
   let def' = if set then Set else Dfv
   -- e' <- sliceExp' e eLbl
   vars <- get; put (vars \\ [name var]); return $ def' var e s
sliceAssignment var e s set (BindingS True _)  = do 
   let def' = if set then Set else Dfv
   vars <- get
   if ((name var) `elem` vars) && (not set)
      then do put (vars \\ [name var]); return $ def' var (dummyExp (spanOf e)) s
      else do put (vars \\ [name var]); return (Nll s) 

sliceBegin :: [Exp] -> Span -> ToSlice -> SliceState 
sliceBegin es s (BeginS True _) = return (Nll s) 
sliceBegin es s (BeginS False lbls) = do
   vars <- get
   es' <- mapM (\(e, l) -> sliceExp' e l) (zip es lbls)
   let es'' = (filter (\e -> not $ isNll e) (init es')) ++ [last es']
   return $ Bgn es'' s

sliceIf :: Exp -> Exp -> Exp -> Span -> ToSlice -> SliceState
sliceIf b c a s (IfS True _ _) = return (Nll s) 
sliceIf b c a s (IfS False lblC lblA) = do 
   c' <- sliceExp' c lblC 
   a' <- sliceExp' a lblA
   return $ Iff b c' a' s

-- label expressions with True if they could be sliced away and False if they definitely need to stay
-- while also gathering all used variables (whose definitions need to stay)

type LabelIrrState = State (AbstractSto V, UsedVars) ToSlice

labelIrrelevant :: Exp -> Labels -> (ToSlice, UsedVars)
labelIrrelevant e l = (e', vs) where (e', (_, vs)) = runState (labelIrrelevant' e l) (mempty, [])

labelIrrelevant' :: Exp -> Labels -> LabelIrrState 
labelIrrelevant' e l = if isSkip l 
   then return (SkipS True) 
   else if isVal l 
      then  do 
         (s, used) <- get 
         let used' = map name $ getVarsFromExp' e
         put (s, union used used')
         return (SkipS False)
      else labelIrrExp' e l

labelIrrExp' :: Exp -> Labels -> LabelIrrState 
labelIrrExp' e@(Let bds bdy s) l       = labelIrrLet e bds bdy s Let l
labelIrrExp' e@(Ltt bds bdy s) l       = labelIrrLet e bds bdy s Ltt l 
labelIrrExp' e@(Ltr bds bdy s) l       = labelIrrLet e bds bdy s Ltr l 
labelIrrExp' e@(Lrr bds bdy s) l       = labelIrrLet e bds bdy s Lrr l
labelIrrExp' e@(Dfv var e' _) l        = labelIrrBinding ((var, e'), l)
labelIrrExp' e@(Set var e' _) l        = do  r <- labelIrrBinding ((var, e'), l)
                                             if (sliceBool r) 
                                                then return r
                                                else do
                                                   (s, used) <- get
                                                   let used' = union used [(name var)]
                                                   put (s, used')
                                                   return r

labelIrrExp' (Bgn es s) (Begin lbls)   = do es' <- mapM (uncurry labelIrrelevant') (zip es lbls)
                                            if null es' 
                                               then return $ BeginS True []
                                               else return $ BeginS False es'
labelIrrExp' e@(Iff _ _ _ _) l         = labelIrrIf e l                           

labelIrrLet :: Exp -> [(Ide, Exp)] -> Exp -> Span -> ([(Ide, Exp)] -> Exp -> Span -> Exp) -> Labels -> LabelIrrState
labelIrrLet e bds bdy s let' (Lett g lbls lbl) = do (sto, used) <- get 
                                                    bds' <- mapM labelIrrBinding (zip bds lbls)

                                                    bdy' <- labelIrrelevant' bdy lbl
                                                    if (preserve sto g e)  then do put (sto, used); return $ LetS True [] (SkipS True)
                                                                           else return $ LetS False bds' bdy'

labelIrrBinding :: ((Ide, Exp), Labels) -> LabelIrrState
labelIrrBinding ((var, exp), (Binding g lbl)) = 
   do eLbl <- labelIrrelevant' exp lbl
      (s, used) <- get
      let (b, s') = preserveWithSto s g (Dfv var exp NoSpan) 
      let used' = map name $ getVarsFromExp' exp
      if b 
         then do put (s', used); return $ BindingS True (SkipS True)  
         else do put (s', union (used \\ [name var]) used'); return $ BindingS False eLbl     


labelIrrIf :: Exp -> Labels -> LabelIrrState 
labelIrrIf e@(Iff b c a s) (If g lblC lblA) = do   (sto, used) <- get
                                                   c' <- labelIrrelevant' c lblC; (_, usedC) <- get; put (sto, used) 
                                                   a' <- labelIrrelevant' a lblA; (_, usedA) <- get
                                                   let used' = map name $ getVarsFromExp' b
                                                   put (sto, union used' $ union used $ union usedA usedC)
                                                   -- if (preserve sto g e) then do put (sto, used); return (IfS True (SkipS True) (SkipS True)) else return $ IfS False c' a'
                                                   return $ IfS False c' a'