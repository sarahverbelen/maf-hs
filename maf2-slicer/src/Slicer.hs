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
slice p c = sliceExp vars p $ labelSequence p c where vars = getVars c

data UsedVars = LetU [String] [UsedVars] UsedVars | IfU [String] UsedVars UsedVars | BindingU [String] UsedVars | SkipU [String] | BeginU [String] [UsedVars] deriving (Eq, Show)
data ToSlice  = LetS Bool [ToSlice] ToSlice | IfS Bool ToSlice ToSlice | BindingS Bool ToSlice | SkipS Bool | BeginS Bool [ToSlice] deriving (Eq, Show)

getUsedVars :: UsedVars -> [String]
getUsedVars (LetU vs _ _)   = vs 
getUsedVars (IfU vs _ _)    = vs 
getUsedVars (BindingU vs _) = vs 
getUsedVars (SkipU vs)      = vs 
getUsedVars (BeginU vs _)   = vs

sliceBool :: ToSlice -> Bool 
sliceBool (LetS b _ _)   = b
sliceBool (IfS b _ _)    = b 
sliceBool (BindingS b _) = b 
sliceBool (SkipS b)      = b 
sliceBool (BeginS b _)   = b

dummyExp :: Span -> Exp 
dummyExp s = App (Var (Ide "dummy" s)) [Num 5 NoSpan] NoSpan -- todo: use value that corresponds to known abstract value
 
-- | SLICING PASS (front to back)
-- takes the information from the used variables and the irrelevant expressions to slice away irrelevant expressions that don't define a used variable.

sliceExp :: [String] -> Exp -> Labels -> Exp 
sliceExp vars e l = 
   let toslice = labelIrrelevant e l 
       usedvars = findUsedVars e toslice (SkipU vars) 
   in sliceExp' e toslice usedvars 

sliceExp' :: Exp -> ToSlice -> UsedVars -> Exp
sliceExp' e (SkipS True) _             = Nll (spanOf e)
sliceExp' (Let bds bdy s) toslice used = sliceLet bds bdy s Let toslice used
sliceExp' (Ltt bds bdy s) toslice used = sliceLet bds bdy s Ltt toslice used
sliceExp' (Ltr bds bdy s) toslice used = sliceLet bds bdy s Ltr toslice used
sliceExp' (Lrr bds bdy s) toslice used = sliceLet bds bdy s Lrr toslice used
sliceExp' (Dfv var e s)   toslice used = sliceAssignment var e s False toslice used
sliceExp' (Set var e s)   toslice used = sliceAssignment var e s True toslice used
sliceExp' (Bgn es s)      toslice used = sliceBegin es s toslice used
sliceExp' (Iff b c a s)   toslice used = sliceIf b c a s toslice used
sliceExp' e _ _                        = e

sliceLet :: [(Ide, Exp)] -> Exp -> Span -> ([(Ide, Exp)] -> Exp -> Span -> Exp) -> ToSlice -> UsedVars -> Exp  
sliceLet bds bdy s let' (LetS True _ _) _ = Nll s
sliceLet bds bdy s let' (LetS False lblBds lblBdy) (LetU vars varsBds varsBdy) =
   let bds' = sliceBinds bds lblBds varsBds
       bdy' = sliceExp' bdy lblBdy varsBdy
       sliceCompletely = (isNll bdy') && (null bds')
   in if sliceCompletely then Nll s else let' bds' bdy' s                                                                                        

sliceBinds :: [(Ide, Exp)] -> [ToSlice] -> [UsedVars] -> [(Ide, Exp)] 
sliceBinds [] _ _ = []
sliceBinds (bd:bds) (l@(BindingS _ eLbl):toslice) ((BindingU vars varsE):used) = 
   let b = sliceBind bd l vars 
       (var, e) = bd
       nextBds = sliceBinds bds toslice used
      --  e' = e
       e' = sliceExp' e eLbl varsE
   in if (b == (Just True)) 
         then nextBds -- slice the binding completely
         else if (b == Nothing) 
            then [(var, e')] ++ nextBds -- keep the binding
            else [(var, dummyExp (spanOf e))] ++ nextBds -- dummify the binding
sliceBinds (bd:bds) (l@(BindingS _ eLbl):toslice) used = error $ show used           

sliceBind :: (Ide, Exp) -> ToSlice -> [String] -> Maybe Bool 
sliceBind bd (BindingS False _) vars = Nothing -- this binding is necessary
sliceBind (var, e) (BindingS True _) vars = if (name var) `elem` vars 
   then Just False -- this binding could be sliced away but it is used in some other expression so we need to dummify it
   else Just True -- this binding can be sliced away

sliceAssignment :: Ide -> Exp -> Span -> Bool -> ToSlice -> UsedVars -> Exp
sliceAssignment var e s set (BindingS False eLbl) (BindingU _ varsE) = 
   let def' = if set then Set else Dfv
      --  e' = e
       e' = sliceExp' e eLbl varsE
   in def' var e' s
sliceAssignment var e s set (BindingS True _) used =
   let def' = if set then Set else Dfv
       vars = getUsedVars used
   in if ((name var) `elem` vars) && (not set)
         then def' var (dummyExp (spanOf e)) s
         else Nll s

sliceBegin :: [Exp] -> Span -> ToSlice -> UsedVars -> Exp
sliceBegin es s (BeginS True _) _ = Nll s
sliceBegin es s (BeginS False lbls) (BeginU _ varsE) =
   let es' = map (uncurry $ uncurry sliceExp') (zip (zip es lbls) varsE)
       es'' = (filter (\e -> not $ isNll e) es')
   in if null es'' then Nll s else Bgn es'' s    

sliceIf :: Exp -> Exp -> Exp -> Span -> ToSlice -> UsedVars -> Exp
sliceIf b c a s (IfS True _ _) _ = Nll s
sliceIf b c a s (IfS False lblC lblA) (IfU _ varsC varsA) =
   let c' = sliceExp' c lblC varsC
       a' = sliceExp' a lblA varsA    
   in if (isNll c' && isNll a') then Nll s else Iff b c' a' s

-- | USED VARIABLE ANALYSIS PASS (back to front, single-pass live variable analysis)
-- to find out what variables need to keep their initial defines
findUsedVars :: Exp -> ToSlice -> UsedVars -> UsedVars 
findUsedVars e (SkipS True)          used = used
findUsedVars (Let bds bdy s) toslice used = findUsedVarsLet bds bdy toslice used
findUsedVars (Ltt bds bdy s) toslice used = findUsedVarsLet bds bdy toslice used 
findUsedVars (Ltr bds bdy s) toslice used = findUsedVarsLet bds bdy toslice used
findUsedVars (Lrr bds bdy s) toslice used = findUsedVarsLet bds bdy toslice used
findUsedVars (Dfv var e s)   toslice used = findUsedVarsBinding var e False toslice used
findUsedVars (Set var e s)   toslice used = findUsedVarsBinding var e True toslice used
findUsedVars (Bgn es s)      toslice used = findUsedVarsBegin es toslice used
findUsedVars (Iff b c a s)   toslice used = findUsedVarsIf b c a toslice used  
findUsedVars e               _       used = SkipU ((getVarsFromExp' e) `union` (getUsedVars used))

findUsedVarsLet :: [(Ide, Exp)] -> Exp -> ToSlice -> UsedVars -> UsedVars 
findUsedVarsLet _ _ (LetS True _ _) used = used 
findUsedVarsLet bds bdy (LetS False lblBds lblBdy) used = 
   let usedVarsBdy = findUsedVars bdy lblBdy used
       usedVarsBds = reverse $ findUsedVarsBindings (reverse bds) (reverse lblBds) usedVarsBdy
       varsBdy = getUsedVars usedVarsBdy
       varsBds = if null usedVarsBds then [] else getUsedVars $ head usedVarsBds
   in LetU (union varsBds varsBdy) usedVarsBds usedVarsBdy
        
findUsedVarsBindings :: [(Ide, Exp)] -> [ToSlice] -> UsedVars -> [UsedVars]
findUsedVarsBindings [] _ _ = []
findUsedVarsBindings ((var, e):bds) (toslice:lblBds) used = 
   let usedVars = findUsedVarsBinding var e False toslice used 
       nextVars = findUsedVarsBindings bds lblBds usedVars 
   in (usedVars:nextVars)
 
findUsedVarsBinding :: Ide -> Exp -> Bool -> ToSlice -> UsedVars -> UsedVars
findUsedVarsBinding var _ _ (BindingS True _) used = BindingU (getUsedVars used) (SkipU [])
findUsedVarsBinding var e isSet (BindingS False toslice) used = 
   let prevUsed = getUsedVars used 
       varsExp = findUsedVars e toslice used
       usedInExp = getUsedVars varsExp
       usedVars = if isSet then prevUsed `union` usedInExp `union` [(name var)] else (prevUsed \\ [name var]) `union` usedInExp
   in BindingU usedVars varsExp

findUsedVarsBegin :: [Exp] -> ToSlice -> UsedVars -> UsedVars
findUsedVarsBegin _ (BeginS True _) used = used
findUsedVarsBegin es (BeginS False lbls) used = 
   let usedVarsEs = reverse $ findUsedVarsExps (reverse es) (reverse lbls) used
       usedVars = if null usedVarsEs then [] else getUsedVars $ head usedVarsEs
   in BeginU (usedVars `union` (getUsedVars used)) usedVarsEs    

findUsedVarsExps :: [Exp] -> [ToSlice] -> UsedVars -> [UsedVars]
findUsedVarsExps [] _ _ = []
findUsedVarsExps (e:es) (lbl:lbls) used = 
   let usedVars = findUsedVars e lbl used 
       nextVars = findUsedVarsExps es lbls usedVars
   in (usedVars:nextVars)

findUsedVarsIf :: Exp -> Exp -> Exp -> ToSlice -> UsedVars -> UsedVars
findUsedVarsIf _ _ _ (IfS True _ _) used = used
findUsedVarsIf b c a (IfS False lblC lblA) used = 
   let   usedB = getVarsFromExp' b
         usedC = findUsedVars c lblC used 
         usedA = findUsedVars a lblA used 
         usedIf = usedB `union` (getUsedVars usedC) `union` (getUsedVars usedA)
   in IfU usedIf usedC usedA      

-- | FIND IRRELEVANT EXPRESSIONS PASS (front to back)
-- label expressions with True if they could be sliced away and False if they definitely need to stay

type LabelIrrState = State (AbstractSto V) ToSlice

labelIrrelevant :: Exp -> Labels -> ToSlice
labelIrrelevant e l = e' where (e', _) = runState (labelIrrelevant' e l) mempty

labelIrrelevant' :: Exp -> Labels -> LabelIrrState 
labelIrrelevant' e l = if isSkip l 
   then return (SkipS True) 
   else if isVal l 
      then return (SkipS False)
      else labelIrrExp' e l

labelIrrExp' :: Exp -> Labels -> LabelIrrState 
labelIrrExp' e@(Let bds bdy s) l       = labelIrrLet e bds bdy s Let l
labelIrrExp' e@(Ltt bds bdy s) l       = labelIrrLet e bds bdy s Ltt l 
labelIrrExp' e@(Ltr bds bdy s) l       = labelIrrLet e bds bdy s Ltr l 
labelIrrExp' e@(Lrr bds bdy s) l       = labelIrrLet e bds bdy s Lrr l
labelIrrExp' e@(Dfv var e' _) l        = labelIrrBinding ((var, e'), l)
labelIrrExp' e@(Set var e' _) l        = labelIrrBinding ((var, e'), l)
labelIrrExp' (Bgn es s) (Begin lbls)   = do es' <- mapM (uncurry labelIrrelevant') (zip es lbls)
                                            if null es' 
                                               then return $ BeginS True []
                                               else return $ BeginS False es'
labelIrrExp' e@(Iff _ _ _ _) l         = labelIrrIf e l    
labelIrrExp' _ (Val _)                 = return $ SkipS False                       

labelIrrLet :: Exp -> [(Ide, Exp)] -> Exp -> Span -> ([(Ide, Exp)] -> Exp -> Span -> Exp) -> Labels -> LabelIrrState
labelIrrLet e bds bdy s let' (Lett lbls lbl)   = do bds' <- mapM labelIrrBinding (zip bds lbls)
                                                    sto <- get 
                                                   --  error $ (show sto) ++ (show bds')
                                                    bdy' <- labelIrrelevant' bdy lbl 
                                                    return $ LetS False bds' bdy'

labelIrrBinding :: ((Ide, Exp), Labels) -> LabelIrrState
labelIrrBinding ((var, exp), (Binding g lbl)) = 
   do eLbl <- labelIrrelevant' exp lbl
      let eLbl' = relabelIrrBindingExp eLbl
      s <- get
      let (b, s') = preserveWithSto s g (Dfv var exp NoSpan) 
      put s'
      if b 
         then do return $ BindingS True eLbl'  
         else do return $ BindingS False eLbl'  

relabelIrrBindingExp :: ToSlice -> ToSlice 
-- relabels the expression bound to a variable so that the return value is kept 
relabelIrrBindingExp (SkipS _) = SkipS False 
relabelIrrBindingExp (BeginS _ es) = BeginS False $ (init es) ++ [(relabelIrrBindingExp $ last es)]
relabelIrrBindingExp (IfS _ c a) = IfS False (relabelIrrBindingExp c) (relabelIrrBindingExp a)
relabelIrrBindingExp (LetS _ bds bdy) = LetS False bds (relabelIrrBindingExp bdy)
relabelIrrBindingExp l = l                 

labelIrrIf :: Exp -> Labels -> LabelIrrState 
labelIrrIf e@(Iff b c a s) (If g lblC lblA) = do   sto <- get
                                                   c' <- labelIrrelevant' c lblC; put sto 
                                                   a' <- labelIrrelevant' a lblA; 
                                                   put sto
                                                   return $ IfS False c' a'