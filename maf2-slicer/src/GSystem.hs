{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module GSystem(labelSequence, LabeledExp) where 

import Property.Agreement 
import Property.Preservation
import Dependency.State
import Syntax.Scheme.AST

import Control.Monad.State

type LabeledExp = Exp

-- label all statements in the sequence with agreements by backwards propagating the G-system rules
labelSequence :: Exp -> Agreement -> LabeledExp
labelSequence e initialG = undefined

labelExp :: forall v . (Eq v) => Exp -> (AbstractSto v -> Bool) -> Agreement -> Agreement
-- | implements the rules as described in the G-system
-- | G-PP
labelExp e p g = if (preserve @v p g e) then g else (evalState (labelExp' e) (p, g))


labelExp' :: forall v . (Eq v) => Exp -> State ((AbstractSto v -> Bool), Agreement) Agreement
-- | G-CONCAT
labelExp' (Bgn (e':es) x) = do 
    (p, g) <- get
    let g' = labelExp @v (Bgn es x) p g 
    return $ labelExp @v e' p g' -- todo: fix propagation of the predicate
-- | G-SUB TODO?
-- | G-ASSIGN TODO
labelExp' (Dfv var e _) = do (_, g) <- get; return g 
labelExp' (Dff var args bdy _) = do (_, g) <- get; return g 
labelExp' (Set var e x) = do (_, g) <- get; return g 
-- | G-IF1: agreement holds no matter which path is taken

-- | G-IF2
-- | G-LET * 
-- | G-APP *
-- | G-SKIP
labelExp' _ = do (_, g) <- get; return g 