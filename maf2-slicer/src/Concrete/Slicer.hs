module Concrete.Slicer where 

import Labels (Labels(..), isVal, isSkip, deleteFromAL, shiftLabels)
import Dependency.Lattice (V, dummyValue)
import Property.Agreement
import Dependency.State

import Syntax.Scheme.AST

import Control.Monad.State
import Data.List (union, delete, (\\))
import qualified Data.Map as Map

slice :: Exp -> Agreement -> Exp
-- | takes a program and a criterion (= agreement (= list of relevant variables)) and returns the program where only statements affecting the criterion are remaining
slice p c = sliceExp vars p $ labelSequence p c where vars = getVars c 

sliceExp :: [String] -> Exp -> Labels -> Exp 
sliceExp vars e l = 
   let toslice = labelIrrelevant e l 
       usedvars = findUsedVars e toslice (SkipU vars) 
   in sliceExp' e toslice usedvars 


labelSequence :: Exp -> Agreement -> Labels
-- | label all statements in the sequence with agreements by backwards propagating the G-system rules
labelSequence e g = shiftLabels (evalState (labelExp e) (mempty, g)) g

---------------------------------------------------------------------------------------------------
--- G-SYSTEM RULES
---------------------------------------------------------------------------------------------------

type LabelState = State (AbstractSto V, Agreement) Labels

labelExp :: Exp -> LabelState
-- | G-CONCAT
labelExp (Bgn es _) = do 
    lbls <- sequence $ map labelExp (reverse es) -- label the expressions back to front
    (_, g) <- get -- take the last agreement as the agreement for the whole begin
    return (Begin (reverse lbls))
-- | G-ASSIGN
labelExp (Dfv var e _) = labelBinding (var, e) 
labelExp (Set var e _) = labelBinding (var, e)
-- | G-IF
labelExp (Iff e c a _) = labelIf e c a
-- | G-LET
labelExp (Let bds bdy _) = labelLet bds bdy
labelExp (Ltt bds bdy _) = labelLet bds bdy
labelExp (Ltr bds bdy _) = labelLet bds bdy
labelExp (Lrr bds bdy _) = labelLet bds bdy
-- | G-APP * (we assume the procedure is a primitive)
labelExp e@(App prc ops s) = labelApp prc ops
-- | G-FUNCTIONDEF * 
--labelExp e@(Dff var ags bdy s) = 
-- | G-LAMBDA * 
--labelExp e@(Lam ags bdy s) = 
-- | G-SKIP
labelExp _ = labelSkip

-- | G-LET
labelLet :: [(Ide, Exp)] -> Exp -> LabelState
labelLet bds bdy = do lblBody <- labelExp bdy -- label the body
                      lblBindings <- mapM labelBinding (reverse bds) -- label the bindings in reverse order
                      return (Lett (reverse lblBindings) lblBody)

-- | G-ASSIGN
labelBinding :: (Ide, Exp) -> LabelState
labelBinding (var, e) = do  (sto, g) <- get
                            eLbl <- labelBindingExp var e
                            let g' = if (name var) `elem` (map fst g) then union (deleteFromAL (name var) g) (map (\a -> (a, PAll)) $ getVarsFromExp' e) else g
                            return (Binding g' eLbl)         

-- | G-APP * (assuming the procedure is a primitive)
labelApp :: Exp -> [Exp] -> LabelState 
labelApp _ ops = do 
    lbls <- mapM labelExp (reverse ops)
    (_, g) <- get 
    return (Appl g (reverse lbls))

-- | G-IF
labelIf :: Exp -> Exp -> Exp -> LabelState
labelIf b c a   = do (sto, g) <- get -- improve: update state
                     lblC <- labelExp c
                     (_, gc) <- get
                     put (sto, g) -- improve: update state
                     lblA <- labelExp a
                     (_, ga) <- get
                     let gb = map (\x -> (x, PAll)) $ getVarsFromExp' b -- condition agreement (if agree on gb, same branch taken) (could be more precise)
                     let gIf = union ga $ union gc gb
                     put (sto, gIf)
                     return (If gb lblC lblA)

-- | G-SKIP
labelSkip :: LabelState
labelSkip = do (_, g) <- get; return (Skip g)

makeVal :: Labels -> Labels 
makeVal (Skip g) = Val g 
makeVal l = l

labelBindingExp :: Ide -> Exp -> LabelState 
-- | labels the expressions bound to a variable
-- needs to know what variables are necessary for the return value
labelBindingExp var e = do 
    (sto, g) <- get
    let p = (lookup (name var) g)
    let g' = if (p == Nothing) 
                then findFinalAgreement e g (Just PAll) 
                else findFinalAgreement e g p
    put (sto, union g g') 
    r <- labelExp e 
    return $ makeVal r  

findFinalAgreement :: Exp -> Agreement -> Maybe Property -> Agreement 
-- | finds the agreement necessary to have the same return value for the expression 
-- (aka all variables that have an influence on the return values)
findFinalAgreement (Bgn es _)  g p = findFinalAgreement (last es) g p
findFinalAgreement (Let _ bdy _) g p = findFinalAgreement bdy g p
findFinalAgreement (Ltt _ bdy _) g p = findFinalAgreement bdy g p
findFinalAgreement (Ltr _ bdy _) g p = findFinalAgreement bdy g p
findFinalAgreement (Lrr _ bdy _) g p = findFinalAgreement bdy g p
findFinalAgreement (Iff _ c a _) g p = union (findFinalAgreement c g p) (findFinalAgreement a g p)
findFinalAgreement (Dfv _ _ _)   g p = g 
findFinalAgreement (Set _ _ _)   g p = g 
findFinalAgreement e             g p = map (\a -> (a, PAll)) $ getVarsFromExp' e          



data UsedVars = LetU [String] [UsedVars] UsedVars | IfU [String] UsedVars UsedVars | BindingU [String] UsedVars | SkipU [String] | BeginU [String] [UsedVars] | AppU [String] [UsedVars] deriving (Eq, Show)
data ToSlice  = LetS Bool [ToSlice] ToSlice | IfS Bool ToSlice ToSlice | BindingS V Bool ToSlice | SkipS Bool | BeginS Bool [ToSlice] | AppS Bool [ToSlice] deriving (Eq, Show)

getUsedVars :: UsedVars -> [String]
getUsedVars (LetU vs _ _)   = vs 
getUsedVars (IfU vs _ _)    = vs 
getUsedVars (BindingU vs _) = vs 
getUsedVars (SkipU vs)      = vs 
getUsedVars (BeginU vs _)   = vs
getUsedVars (AppU vs _)     = vs

sliceBool :: ToSlice -> Bool 
sliceBool (LetS b _ _)   = b
sliceBool (IfS b _ _)    = b 
sliceBool (BindingS _ b _) = b 
sliceBool (SkipS b)      = b 
sliceBool (BeginS b _)   = b
sliceBool (AppS b _)     = b 

dummyExp :: Exp -> V -> Span -> Exp 
dummyExp e v s = 
   if simpleExp e 
      then e 
      else App (Var (Ide "dummy" s)) [dummyValue v] NoSpan
 
simpleExp :: Exp -> Bool 
simpleExp (Num _ _) = True
simpleExp (Rea _ _) = True
simpleExp (Str _ _) = True
simpleExp (Sym _ _) = True
simpleExp (Cha _ _) = True
simpleExp (Bln _ _) = True
simpleExp (Nll _) = True
simpleExp _ = False

-- | SLICING PASS (front to back)
-- takes the information from the used variables and the irrelevant expressions to slice away irrelevant expressions that don't define a used variable.

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
sliceExp' (App prc ops s) toslice used = sliceApp prc ops s toslice used
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
sliceBinds (bd:bds) (l@(BindingS val _ eLbl):toslice) ((BindingU vars varsE):used) = 
   let b = sliceBind bd l vars 
       (var, e) = bd
       nextBds = sliceBinds bds toslice used
      --  e' = e
       e' = sliceExp' e eLbl varsE
   in if (b == (Just True)) 
         then nextBds -- slice the binding completely
         else if (b == Nothing) 
            then [(var, e')] ++ nextBds -- keep the binding
            else [(var, dummyExp e val (spanOf e))] ++ nextBds -- dummify the binding        

sliceBind :: (Ide, Exp) -> ToSlice -> [String] -> Maybe Bool 
sliceBind bd (BindingS _ False _) vars = Nothing -- this binding is necessary
sliceBind (var, e) (BindingS _ True _) vars = if (name var) `elem` vars 
   then Just False -- this binding could be sliced away but it is used in some other expression so we need to dummify it
   else Just True -- this binding can be sliced away

sliceAssignment :: Ide -> Exp -> Span -> Bool -> ToSlice -> UsedVars -> Exp
sliceAssignment var e s set (BindingS _ False eLbl) (BindingU _ varsE) = 
   let def' = if set then Set else Dfv
      --  e' = e
       e' = sliceExp' e eLbl varsE
   in def' var e' s
sliceAssignment var e s set (BindingS val True _) used =
   let def' = if set then Set else Dfv
       vars = getUsedVars used
   in if ((name var) `elem` vars) && (not set)
         then def' var (dummyExp e val (spanOf e)) s
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

-- assuming the procedure is a primitive that doesn't modify the state
sliceApp :: Exp -> [Exp] -> Span -> ToSlice -> UsedVars -> Exp 
sliceApp _ _ s (AppS True _) _ = Nll s
sliceApp prc ops s (AppS False lbls) (AppU _ vars) = 
   let ops' = map (uncurry $ uncurry sliceExp') (zip (zip ops lbls) vars)
   in App prc ops' s   

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
findUsedVars (App prc ops s) toslice used = findUsedVarsApp prc ops toslice used
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
findUsedVarsBinding var _ _ (BindingS _ True _) used = BindingU (getUsedVars used) (SkipU [])
findUsedVarsBinding var e isSet (BindingS _ False toslice) used = 
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

findUsedVarsApp :: Exp -> [Exp] -> ToSlice -> UsedVars -> UsedVars 
findUsedVarsApp _ _ (AppS True _) used = used 
findUsedVarsApp _ ops (AppS False lbls) used = 
   let usedVarsOps = reverse $ findUsedVarsExps (reverse ops) (reverse lbls) used 
       usedVars = if null usedVarsOps then [] else getUsedVars $ head usedVarsOps
   in AppU (usedVars `union` (getUsedVars used)) usedVarsOps   

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
labelIrrExp' e@(App _ _ _) l           = labelIrrApp e l 
labelIrrExp' _ (Val _)                 = return $ SkipS False                       

labelIrrLet :: Exp -> [(Ide, Exp)] -> Exp -> Span -> ([(Ide, Exp)] -> Exp -> Span -> Exp) -> Labels -> LabelIrrState
labelIrrLet e bds bdy s let' (Lett lbls lbl)   = do bds' <- mapM labelIrrBinding (zip bds lbls)
                                                    sto <- get 
                                                    bdy' <- labelIrrelevant' bdy lbl 
                                                    return $ LetS False bds' bdy'

labelIrrBinding :: ((Ide, Exp), Labels) -> LabelIrrState
labelIrrBinding ((var, exp), (Binding g lbl)) = 
   do eLbl <- labelIrrelevant' exp lbl
      let eLbl' = relabelIrrBindingExp eLbl
      s <- get
      let (b, s') = (not $ (name var) `elem` (map fst g), s)
      let b' = not $ relevantBindingInExp eLbl'
      let v = getValue $ abstractEval exp s
      return $ BindingS v (b && b') eLbl'

relabelIrrBindingExp :: ToSlice -> ToSlice 
-- relabels the expression bound to a variable so that the return value is kept 
relabelIrrBindingExp (SkipS _) = SkipS False 
relabelIrrBindingExp (BeginS _ es) = BeginS False $ (init es) ++ [(relabelIrrBindingExp $ last es)]
relabelIrrBindingExp (IfS _ c a) = IfS False (relabelIrrBindingExp c) (relabelIrrBindingExp a)
relabelIrrBindingExp (LetS _ bds bdy) = LetS False bds (relabelIrrBindingExp bdy)
relabelIrrBindingExp (AppS _ lbls) = AppS False (map relabelIrrBindingExp lbls)
relabelIrrBindingExp l = l                 

labelIrrIf :: Exp -> Labels -> LabelIrrState 
labelIrrIf e@(Iff b c a s) (If g lblC lblA) = do   sto <- get
                                                   c' <- labelIrrelevant' c lblC; put sto 
                                                   a' <- labelIrrelevant' a lblA; 
                                                   put sto
                                                   return $ IfS False c' a'

labelIrrApp :: Exp -> Labels -> LabelIrrState 
labelIrrApp e@(App prc ops s) (Appl g lbls) = do sto <- get 
                                                 ops' <- mapM (uncurry labelIrrelevant') (zip ops lbls)
                                                 let ops'' = (map relabelIrrBindingExp ops')
                                                 let b = not $ or $ map relevantBindingInExp ops''
                                                 return $ AppS b ops''                                               

relevantBindingInExp :: ToSlice -> Bool 
relevantBindingInExp (BindingS _ False _) = True 
relevantBindingInExp (BeginS _ es) = or $ map relevantBindingInExp es 
relevantBindingInExp (IfS _ c a) = (relevantBindingInExp c) || (relevantBindingInExp a)
relevantBindingInExp (LetS False _ _) = True 
relevantBindingInExp (AppS _ lbls) = or $ map relevantBindingInExp lbls 
relevantBindingInExp _ = False                                                
