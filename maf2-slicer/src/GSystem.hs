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
labelSequence e g = snd $ execState (labelExp @v e g) ((const True), [])

labelExp :: forall v . (Eq v) => Exp -> Agreement -> State ((AbstractSto v -> Bool), Labels) Agreement
-- | G-PP TODO
-- | G-CONCAT
labelExp (Bgn (e':es) x) g = do 
    g' <- labelExp @v (Bgn es x) g 
    labelExp @v e' g'
-- (| G-SUB TODO?)
-- | G-ASSIGN TODO
labelExp (Dfv var e s) g = do modify (\(p, lbls) -> (p, (s, g):lbls)); return g 
labelExp (Dff var args bdy s) g = do modify (\(p, lbls) -> (p, (s, g):lbls)); return g 
labelExp (Set var e s) g = do modify (\(p, lbls) -> (p, (s, g):lbls)); return g 
-- | G-IF TODO
-- | G-IF2
labelExp (Iff e a b s) g = do modify (\(p, lbls) -> (p, (s, g):lbls)); return g 
-- (| G-IF1: agreement holds no matter which path is taken (TODO: do I need both if rules? or could I get away with only if2 and take the hit to accuracy))
--labelExp' (Iff e a b s) g = do modify (\(p, lbls) -> (p, (s, g):lbls)); return g 
-- | G-LET * TODO
labelExp (Let bds bdy s) g = do modify (\(p, lbls) -> (p, (s, g):lbls)); return g 
labelExp (Ltt bds bdy s) g = do modify (\(p, lbls) -> (p, (s, g):lbls)); return g 
labelExp (Ltr bds bdy s) g = do modify (\(p, lbls) -> (p, (s, g):lbls)); return g 
labelExp (Lrr bds bdy s) g = do modify (\(p, lbls) -> (p, (s, g):lbls)); return g 
-- | G-APP * TODO
labelExp (App prc ops s) g = do modify (\(p, lbls) -> (p, (s, g):lbls)); return g 
-- | G-SKIP
labelExp e g = do modify (\(p, lbls) -> (p, (spanOf e, g):lbls)); return g 