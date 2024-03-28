{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module GSystem(labelSequence, Labels) where 

import Property.Agreement 
import Property.Preservation
import Dependency.State
import Dependency.Lattice
import Syntax.Scheme.AST

import Control.Monad.State
import Data.Tuple.Extra (snd3)
import Data.List (union, subsequences, nubBy, delete)
import qualified Data.Map as Map

type Labels = [Agreement]

type LabelState v = State (AbstractSto v, Labels, Agreement) Agreement

labelSequence :: forall v . (RefinableLattice v) => Exp -> Agreement -> Labels
-- | label all statements in the sequence with agreements by backwards propagating the G-system rules
labelSequence e g = snd3 $ execState (labelExp' @v e) (mempty, [], g)

-- | G-PP
labelExp' :: forall v . (RefinableLattice v) => Exp -> LabelState v
labelExp' e = do 
    (sto, lbls, g) <- get
    if (preserve sto g e)
        then do put (sto, g:lbls, g); return g
        else do labelExp e

labelExp :: forall v . (RefinableLattice v) => Exp -> LabelState v
-- | G-CONCAT
labelExp (Bgn es _) = do 
    sequence_ $ map labelExp' (reverse es) -- label the expressions back to front
    (sto, lbl, g) <- get -- take the last label as the label for the whole begin
    put (sto, g:lbl, g)
    return g
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
labelLet bds bdy = do _ <- labelExp' bdy -- label the body
                      sequence_ $ map labelBinding (reverse bds) -- label the bindings in reverse order
                      (sto, lbls, g) <- get 
                      put (sto, g:lbls, g) -- label the let itself
                      return g

-- | G-ASSIGN
labelBinding :: forall v . (RefinableLattice v) => (Ide, Exp) -> LabelState v
labelBinding (var, e) = do  let ideEq = (\a b -> name a == name b)
                            let vars = nubBy ideEq $ getVarsFromExp e
                            (sto, lbl, g) <- get 
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
                            put (sto', g':lbl, g')
                            return g'

-- | G-IF
labelIf :: forall v . (RefinableLattice v) => Exp -> Exp -> Exp -> LabelState v
labelIf b a c   = do (sto, _, g) <- get -- improve: update state
                     ga <- labelExp' a
                     (_, lbl, _) <- get; put (sto, lbl, g) -- improve: update state
                     gc <- labelExp' c
                     let gb = getVarsFromExp b -- condition agreement (if agree on gb, same branch taken) (could be more precise)
                     let gIf = union ga $ union gc gb
                     (_, lbl', _) <- get; put (sto, gIf:lbl', gIf)
                     return gIf

-- | G-SKIP
labelSkip :: LabelState v
labelSkip = do (sto, lbls, g) <- get; put (sto, g:lbls, g); return g