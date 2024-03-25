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
import Data.List (intersect)

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
-- | G-CONCAT
labelExp (Bgn [] _) = do (_, _, g) <- get; return g
labelExp (Bgn (e':es) x) = do 
    _  <- labelExp' @v (Bgn es x) 
    labelExp' @v e'
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
labelExp e = labelSkip e $ spanOf e

-- | G-LET
labelLet :: forall v. (Eq v) =>  [(Ide, Exp)] -> Exp -> Span -> LabelState v
labelLet bds bdy s = do _ <- sequence $ map labelBinding (reverse bds) 
                        g <- labelExp' bdy
                        return g

-- | G-ASSIGN TODO 
labelBinding :: forall v . (Eq v) => (Ide, Exp) -> LabelState v
labelBinding (var, e) = labelSkip e $ spanOf e

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
labelSkip :: Exp -> Span -> LabelState v
labelSkip e s = do (sto, lbls, g) <- get; put (sto, (s, g):lbls, g); return g