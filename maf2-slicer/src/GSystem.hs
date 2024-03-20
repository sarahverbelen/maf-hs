{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module GSystem(labelSequence, Labels) where 

import Property.Agreement 
import Property.Preservation
import Dependency.State
import Syntax.Scheme.AST

import Control.Monad.State

type Labels = [(Span, Agreement)]

-- label all statements in the sequence with agreements by backwards propagating the G-system rules
labelSequence :: forall v . (Eq v) => Exp -> Agreement -> Labels
labelSequence e initialG = labelExp @v e (const True) initialG 

labelExp :: forall v . (Eq v) => Exp -> (AbstractSto v -> Bool) -> Agreement -> Labels
-- | implements the rules as described in the G-system
-- | G-PP
labelExp e p g = if (preserve @v p g e) then [(spanOf e, g)] else (snd $ execState (labelExp' e g) (p, [(spanOf e, g)]))


labelExp' :: forall v . (Eq v) => Exp -> Agreement -> State ((AbstractSto v -> Bool), Labels) Agreement
-- | G-CONCAT
labelExp' (Bgn (e':es) x) g = do 
    g' <- labelExp' @v (Bgn es x) g 
    labelExp' @v e' g' -- todo: fix propagation of the predicate
-- | G-SUB TODO?
-- | G-ASSIGN TODO
labelExp' (Dfv var e _) g = do (_, lbls) <- get; return g 
labelExp' (Dff var args bdy _) g = do (_, lbls) <- get; return g 
labelExp' (Set var e x) g = do (_, lbls) <- get; return g 
-- | G-IF1: agreement holds no matter which path is taken (TODO: do I need both if rules? or could I get away with only if2 and take the hit to accuracy)
labelExp' (Iff e a b _) g = do (_, lbls) <- get; return g 
-- | G-IF2
-- | G-LET * 
labelExp' (Let bds bdy _) g = do (_, lbls) <- get; return g 
labelExp' (Ltt bds bdy _) g = do (_, lbls) <- get; return g 
labelExp' (Ltr bds bdy _) g = do (_, lbls) <- get; return g 
labelExp' (Lrr bds bdy _) g = do (_, lbls) <- get; return g 
-- | G-APP *
labelExp' (App prc ops _) g = do (_, lbls) <- get; return g 
-- | G-SKIP
labelExp' _ g = do (_, lbls) <- get; return g 