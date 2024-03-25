{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module GSystem(labelSequence, Labels) where 

import Property.Agreement 
import Property.Preservation
import Dependency.State
import Syntax.Scheme.AST

import Control.Monad.State
import Data.Tuple.Extra (snd3)
import Data.List (intersect, subsequences, nubBy, deleteBy)

type Labels = [(Span, Agreement)]

type LabelState v = State (AbstractSto v, Labels, Agreement) Agreement

-- label all statements in the sequence with agreements by backwards propagating the G-system rules
labelSequence :: forall v . (Eq v) => Exp -> Agreement -> Labels
labelSequence e g = snd3 $ execState (labelExp' @v e) (mempty, [], g)

-- | G-PP
labelExp' :: forall v . (Eq v) => Exp -> LabelState v
labelExp' e = do 
    (sto, lbls, g) <- get
    if (preserve sto g e)
        then do put (sto, (spanOf e, g):lbls, g); return g
        else do labelExp e

labelExp :: forall v . (Eq v) => Exp -> LabelState v
-- | G-CONCAT TODO: fix?
labelExp (Bgn es s) = do 
    sequence_ $ map labelExp' (reverse es) -- label the expressions back to front
    (sto, lbl, g) <- get -- take the last label as the label for the whole begin
    put (sto, (s, g):lbl, g)
    return g
-- | G-ASSIGN
labelExp (Dfv var e s) = labelBinding (var, e) 
labelExp (Set var e s) = labelBinding (var, e)
--labelExp (Dff var args bdy s) g = TODO?
-- | G-IF
labelExp (Iff e a b s) = labelIf e a b s
-- | G-LET
labelExp (Let bds bdy s) = labelLet bds bdy s
labelExp (Ltt bds bdy s) = labelLet bds bdy s
labelExp (Ltr bds bdy s) = labelLet bds bdy s
labelExp (Lrr bds bdy s) = labelLet bds bdy s
--( | G-APP TODO
--labelExp e@(App prc ops s) =)
-- | G-SKIP
labelExp e = labelSkip $ spanOf e

-- | G-LET
labelLet :: forall v. (Eq v) =>  [(Ide, Exp)] -> Exp -> Span -> LabelState v
labelLet bds bdy s = do sequence_ $ map labelBinding (reverse bds)
                        labelExp' bdy

-- | G-ASSIGN
labelBinding :: forall v . (Eq v) => (Ide, Exp) -> LabelState v
labelBinding (var, e) = do  --labelExp' e TODO: check if necessary? 
                            let ideEq = (\a b -> name a == name b)
                            let vars = nubBy ideEq $ getVarsFromExp e
                            (sto, lbl, g) <- get 
                            let gs = [g' | g' <- subsequences $ g ++ vars] -- all possible agreements
                            -- filter such that given two states that agree on g', the abstract value of the expression agrees on g
                            -- (if the variable being assigned is not in the agreement, the agreement will be unchanged)
                            let gs' = if any (ideEq var) g then filter (allStatesAgreeOn e sto) gs else [g]
                            -- take the first one (not necessarily the smallest because of how subsequences works)
                            let g' = head gs'
                            put (sto, (spanOf e, g'):lbl, g')
                            return g'

-- | G-IF
labelIf :: forall v . (Eq v) => Exp -> Exp -> Exp -> Span -> LabelState v
labelIf e a c s = do (sto, _, g) <- get -- todo: update state
                     ga <- labelExp' a
                     (_, lbl, _) <- get; put (sto, lbl, g) -- todo: update state
                     gc <- labelExp' c
                     let gb = intersect ga gc
                     (_, lbl', _) <- get; put (sto, (s, gb):lbl', gb)
                     return gb

-- | G-SKIP
labelSkip :: Span -> LabelState v
labelSkip s = do (sto, lbls, g) <- get; put (sto, (s, g):lbls, g); return g



getVarsFromExp :: Exp -> [Ide]
getVarsFromExp (Var x)             = [x]
getVarsFromExp (Iff b a c _)       = getVarsFromExp b ++ getVarsFromExp a ++ getVarsFromExp c
getVarsFromExp (Lam prs bdy _)     = prs ++ getVarsFromExp bdy
getVarsFromExp (Bgn es _)          = foldr (\e l -> l ++ getVarsFromExp e) [] es
getVarsFromExp (Dfv var e _)       = [var] ++ getVarsFromExp e
getVarsFromExp (Dff var prs bdy _) =  [var] ++ prs ++ getVarsFromExp bdy
getVarsFromExp (Set var e _)       = [var] ++ getVarsFromExp e
getVarsFromExp (Let bds bdy _)     = map fst bds ++ getVarsFromExp bdy
getVarsFromExp (Ltt bds bdy _)     = map fst bds ++ getVarsFromExp bdy
getVarsFromExp (Ltr bds bdy _)     = map fst bds ++ getVarsFromExp bdy
getVarsFromExp (Lrr bds bdy _)     = map fst bds ++ getVarsFromExp bdy
getVarsFromExp (App op ops _)      = foldr (\e l -> l ++ getVarsFromExp e) [] ops -- ++ getVarsFromExp op
getVarsFromExp _                   = []