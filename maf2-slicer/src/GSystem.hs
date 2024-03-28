{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module GSystem(labelSequence, Labels(..)) where 

import Property.Agreement 
import Property.Preservation
import Dependency.State
import Dependency.Lattice
import Syntax.Scheme.AST

import Control.Monad.State
import Data.Tuple.Extra (snd3)
import Data.List (union, subsequences, nubBy, delete)
import qualified Data.Map as Map

data Labels = Lett Agreement [Labels] Labels | If Agreement Labels Labels | Binding Agreement | Skip Agreement | Begin Agreement [Labels] deriving (Show, Eq)

type LabelState v = State (AbstractSto v, Agreement) Labels

labelSequence :: forall v . (RefinableLattice v) => Exp -> Agreement -> Labels
-- | label all statements in the sequence with agreements by backwards propagating the G-system rules
labelSequence e g = evalState (labelExp' @v e) (mempty, g)

-- | G-PP
labelExp' :: forall v . (RefinableLattice v) => Exp -> LabelState v
labelExp' e = do 
    (sto, g) <- get
    if (preserve sto g e)
        then do put (sto, g); return (Skip g)
        else do labelExp e

labelExp :: forall v . (RefinableLattice v) => Exp -> LabelState v
-- | G-CONCAT
labelExp (Bgn es _) = do 
    lbls <- sequence $ map labelExp' (reverse es) -- label the expressions back to front
    (_, g) <- get -- take the last agreement as the agreement for the whole begin
    return (Begin g (reverse lbls))
-- | G-ASSIGN
labelExp (Dfv var e _) = labelBinding (var, e) 
labelExp (Set var e _) = labelBinding (var, e)
-- | G-IF
labelExp (Iff e a b _) = labelIf e a b
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
labelLet :: forall v. (RefinableLattice v) =>  [(Ide, Exp)] -> Exp -> LabelState v
labelLet bds bdy = do lblBody <- labelExp' bdy -- label the body
                      lblBindings <- sequence $ map labelBinding (reverse bds) -- label the bindings in reverse order
                      (_, g) <- get 
                      return (Lett g (reverse lblBindings) lblBody)

-- | G-ASSIGN
labelBinding :: forall v . (RefinableLattice v) => (Ide, Exp) -> LabelState v
labelBinding (var, e) = do  let ideEq = (\a b -> name a == name b)
                            let vars = nubBy ideEq $ getVarsFromExp e
                            (sto, g) <- get 
                            -- we need to figure out what variables the expression is dependent on
                            let gs = [g' | g' <- subsequences $ vars] 
                            -- if the current variable being assigned is not in the agreement, then the agreement will be unchanged
                            -- otherwise, we check which variables need to have the same abstract value to have the same result for the expression
                            -- these variables will then be in the new agreement
                            let gs' = if any (ideEq var) g then filter (allStatesAgreeOn e sto) gs else [g]
                            let g' = union (delete var g) $ head gs' -- the new agreement needs to be as precise as g on all variables except the current one being assigned
                            let v = abstractEvalWithState sto e
                            -- update the state
                            let sto' = Map.insert var v sto
                            put (sto', g')
                            return (Binding g')

-- | G-IF
labelIf :: forall v . (RefinableLattice v) => Exp -> Exp -> Exp -> LabelState v
labelIf b c a   = do (sto, g) <- get -- improve: update state
                     lblC <- labelExp' c
                     (_, gc) <- get
                     put (sto, g) -- improve: update state
                     lblA <- labelExp' a
                     (_, ga) <- get
                     let gb = getVarsFromExp b -- condition agreement (if agree on gb, same branch taken) (could be more precise)
                     let gIf = union ga $ union gc gb
                     put (sto, gIf)
                     return (If gIf lblA lblC)

-- | G-SKIP
labelSkip :: LabelState v
labelSkip = do (sto, g) <- get; return (Skip g)