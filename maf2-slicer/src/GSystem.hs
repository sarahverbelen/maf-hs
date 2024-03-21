{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module GSystem(labelSequence, Labels) where 

import Property.Agreement 
import Property.Preservation
import Dependency.State
import Syntax.Scheme.AST

import Control.Monad.State
import Data.Tuple.Extra(snd3)

type Labels = [(Span, Agreement)]

-- label all statements in the sequence with agreements by backwards propagating the G-system rules
labelSequence :: forall v . (Eq v) => Exp -> Agreement -> Labels
labelSequence e g = snd3 $ execState (labelExp @v e) ((const True), [], g)

labelExp :: forall v . (Eq v) => Exp -> State ((AbstractSto v -> Bool), Labels, Agreement) Agreement
-- | G-PP TODO
-- | G-CONCAT
labelExp (Bgn (e':es) x) = do 
    labelExp @v (Bgn es x) 
    labelExp @v e'
-- | G-ASSIGN TODO
labelExp (Dfv var e s) = labelBinding (var, e, s) 
labelExp (Set var e s) = labelBinding (var, e, s)
--labelExp (Dff var args bdy s) g = 
-- | G-IF
labelExp (Iff e a b s) = labelIf e a b s
-- | G-LET
labelExp (Let bds bdy s) = labelLet bds bdy s
labelExp (Ltt bds bdy s) = labelLet bds bdy s
labelExp (Ltr bds bdy s) = labelLet bds bdy s
labelExp (Lrr bds bdy s) = labelLet bds bdy s
-- | G-APP * TODO
labelExp (App prc ops s) = do modify (\(p, lbls, g) -> (p, (s, g):lbls, g)); (_, _, g) <- get; return g 
-- | G-SKIP
labelExp e = do modify (\(p, lbls, g) -> (p, (spanOf e, g):lbls, g)); (_, _, g) <- get; return g 

-- | G-LET TODO
labelLet :: forall v. (Eq v) =>  [(Ide, Exp)] -> Exp -> Span -> State ((AbstractSto v -> Bool), Labels, Agreement) Agreement
labelLet bds bdy s = do modify (\(p, lbls, g) -> (p, (s, g):lbls, g)); (_, _, g) <- get; return g  
 
-- | G-ASSIGN TODO
labelBinding :: forall v . (Eq v) => (Ide, Exp, Span) -> State ((AbstractSto v -> Bool), Labels, Agreement) Agreement
labelBinding (var, e, s) = do modify (\(p, lbls, g) -> (p, (s, g):lbls, g)); (_, _, g) <- get; return g 

-- | G-IF TODO
labelIf :: forall v . (Eq v) => Exp -> Exp -> Exp -> Span -> State ((AbstractSto v -> Bool), Labels, Agreement) Agreement
labelIf e a b s = do modify (\(p, lbls, g) -> (p, (s, g):lbls, g)); (_, _, g) <- get; return g 