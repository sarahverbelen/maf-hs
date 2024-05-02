{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Labels(labelSequence, Labels(..), isSkip, isVal) where 

import Property.Agreement 
import Property.Preservation
import Dependency.State
import Dependency.Lattice
import Dependency.Dependency

import Syntax.Scheme.AST
import Lattice
import Control.Monad.State

import Data.List (union, delete)
import qualified Data.Map as Map

data Labels = Lett [Labels] Labels | If Agreement Labels Labels | Binding Agreement Labels | Skip Agreement | Begin [Labels]  | Val Agreement | Appl Agreement [Labels] deriving (Show, Eq)

isSkip :: Labels -> Bool 
isSkip (Skip _) = True 
isSkip _ = False

isVal :: Labels -> Bool 
isVal (Val _) = True 
isVal _ = False

labelSequence :: Exp -> Agreement -> Labels
-- | label all statements in the sequence with agreements by backwards propagating the G-system rules
labelSequence e g = shiftLabels (evalState (labelExp e) (mempty, g)) g

-- EXTRA PASS: ensure the labels are assigned to the correct expression
shiftLabels :: Labels -> Agreement -> Labels 
shiftLabels lbls g = evalState (shiftLabels' lbls) g

shiftLabels' :: Labels -> State Agreement Labels
-- | labelSequence assigns a label to each expression, but actually this is the label of the *previous* expression, so 
-- shiftLabels shifts all labels to one expression earlier by taking a label and the previous agreement 
-- and returning the shifted labels and the agreement which was the first one of the sequence
shiftLabels' (Skip g)                   = do    g' <- get; put g
                                                return $ Skip g'
shiftLabels' (Val g)                    = do    g' <- get; put g
                                                return $ Val g'                                                
shiftLabels' (Begin lbls)               = do    lbls' <- mapM shiftLabels' (reverse lbls)
                                                return $ Begin (reverse lbls')
shiftLabels' (Binding g lbl)            = do    g' <- get
                                                lbl' <- shiftLabels' lbl
                                                g'' <- get
                                                let newG = g' `union` g''
                                                return $ Binding newG lbl'
shiftLabels' (If gb lblC lblA)          = do    g <- get
                                                lblC' <- shiftLabels' lblC; gc <- get; put g
                                                lblA' <- shiftLabels' lblA; ga <- get;
                                                let g' = (ga `union` gc) `union` gb
                                                put g'
                                                return $ If gb lblC' lblA'
shiftLabels' (Lett lblBds lblBdy)       = do    lblBdy' <- shiftLabels' lblBdy 
                                                lblBds' <- mapM shiftLabels' (reverse lblBds)
                                                return $ Lett (reverse lblBds') lblBdy'   
shiftLabels' (Appl g' lbls)             = do    lbls' <- mapM shiftLabels' lbls 
                                                g <- get 
                                                put g' 
                                                return $ Appl g lbls'                                                                                          
                                        
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
                            -- if the assigned variable is in the agreement
                            -- then the new agreement contains all variables that this expression is dependent on + all of the previous ones except the current one being assigned
                            let g' = if (name var) `elem` (map fst g) then union (deleteFromAL (name var) g) $ findNDeps e (lookup (name var) g) (extendStateForExp e sto) else g
                            -- else the agreement stays the same
                            --let (_, sto') = abstractEval' (Set var e NoSpan) sto
                            -- update the state
                            put (sto, g')
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
findFinalAgreement e             g p = findNDeps e p (extendStateForExp e mempty)


-- unrelated helper
deleteFromAL :: (Eq key) => key -> [(key, b)] -> [(key, b)]
deleteFromAL _ [] = []
deleteFromAL b ((a, x):r)
    | a == b = deleteFromAL b r 
    | otherwise = (a, x) : deleteFromAL b r