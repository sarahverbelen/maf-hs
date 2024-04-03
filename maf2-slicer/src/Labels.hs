{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Labels(labelSequence, Labels(..), isSkip) where 

import Property.Agreement 
import Property.Preservation
import Dependency.State
import Dependency.Lattice
import Dependency.Dependency
import Syntax.Scheme.AST

import Control.Monad.State
import Data.List (union, delete)
import qualified Data.Map as Map

data Labels = Lett Agreement [Labels] Labels | If Agreement Labels Labels | Binding Agreement | Skip Agreement | Begin [Labels] deriving (Show, Eq)

type LabelState = State (AbstractSto V, Agreement) Labels

isSkip :: Labels -> Bool 
isSkip (Skip _) = True 
isSkip _ = False

shiftLabels :: Labels -> Agreement -> Labels 
shiftLabels lbls g = evalState (shiftLabels' lbls) g

shiftLabels' :: Labels -> State Agreement Labels
-- | labelSequence assigns a label to each expression, but actually this is the label of the *previous* expression, so 
-- shiftLabels shifts all labels to one expression earlier by taking a label and the previous agreement 
-- and returning the shifted labels and the agreement which was the first one of the sequence
shiftLabels' (Skip g)                   = do    g' <- get; put g
                                                return $ Skip g'
shiftLabels' (Begin lbls)               = do    lbls' <- mapM shiftLabels' (reverse lbls)
                                                return $ Begin (reverse lbls')
shiftLabels' (Binding g)                = do    g' <- get; put g
                                                return $ Binding g'
shiftLabels' (If g lblC lblA)           = do    g' <- get
                                                lblA' <- shiftLabels' lblA; put g' 
                                                lblC' <- shiftLabels' lblC; put g 
                                                return $ If g' lblC' lblA'
shiftLabels' (Lett g lblBds lblBdy)     = do    lblBdy' <- shiftLabels' lblBdy 
                                                lblBds' <- mapM shiftLabels' (reverse lblBds)
                                                g' <- get; put g
                                                return $ Lett g' (reverse lblBds') lblBdy'
                                        

labelSequence :: Exp -> Agreement -> Labels
-- | label all statements in the sequence with agreements by backwards propagating the G-system rules
labelSequence e g = shiftLabels (evalState (labelExp' e) (mempty, g)) g

-- | G-PP
labelExp' :: Exp -> LabelState
labelExp' e = do 
    (sto, g) <- get
    if (preserve sto g e)
        then do put (sto, g); return (Skip g)
        else do labelExp e

labelExp :: Exp -> LabelState
-- | G-CONCAT
labelExp (Bgn es _) = do 
    lbls <- sequence $ map labelExp' (reverse es) -- label the expressions back to front
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
-- | G-APP *
--labelExp e@(App prc ops s) =
-- | G-SKIP
labelExp _ = labelSkip

-- | G-LET
labelLet :: [(Ide, Exp)] -> Exp -> LabelState
labelLet bds bdy = do lblBody <- labelExp' bdy -- label the body
                      lblBindings <- sequence $ map labelBinding (reverse bds) -- label the bindings in reverse order
                      (_, g) <- get 
                      return (Lett g (reverse lblBindings) lblBody)

-- | G-ASSIGN
labelBinding :: (Ide, Exp) -> LabelState
labelBinding (var, e) = do  (sto, g) <- get 
                            -- if the assigned variable is in the agreement
                            -- then the new agreement contains all variables that this expression is dependent on + all of the previous ones except the current one being assigned
                            let g' = if (name var) `elem` g then union (delete (name var) g) $ dependencies e sto else g
                            -- else the agreement stays the same
                            let v = abstractEval e sto
                            -- update the state
                            let sto' = Map.insert var v sto
                            put (sto', g')
                            return (Binding g')

-- | G-IF
labelIf :: Exp -> Exp -> Exp -> LabelState
labelIf b c a   = do (sto, g) <- get -- improve: update state
                     lblC <- labelExp' c
                     (_, gc) <- get
                     put (sto, g) -- improve: update state
                     lblA <- labelExp' a
                     (_, ga) <- get
                     let gb = map name $ getVarsFromExp' b -- condition agreement (if agree on gb, same branch taken) (could be more precise)
                     let gIf = union ga $ union gc gb
                     put (sto, gIf)
                     return (If gIf lblC lblA)

-- | G-SKIP
labelSkip :: LabelState
labelSkip = do (_, g) <- get; return (Skip g)