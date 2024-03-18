{-# LANGUAGE FlexibleContexts #-}

module Property.Preservation(preserve) where 

import Property.Agreement
import Dependency.State
import Syntax.Scheme.AST
import Analysis.Scheme

import qualified Data.Map as Map
import Control.Monad.State

-- | PP(g, e)
preserve' :: (SchemeAnalysisConstraints var v ctx dep) => Exp -> State ((AbstractSto v -> Bool), Agreement) Bool -- TODO: can agreement ever change? Do we need to keep track of it?
-- | PP-ASSIGN
preserve' (Dfv var e _) = do b <- preserve' e 
                             (p, _) <- get
                             let (s, v) = abstractEvalWithPredicate p e
                             let b' = v `elem` Map.lookup var s -- check if the property is the same as before the assignment (if the var existed then)
                             return $ b && b'
-- preserve' (Dff var args bdy _) = TODO? does a function have an abstract property we can check?
preserve' (Set var e x) = preserve' (Dfv var e x)
-- | PP-LET * (could modify the state in the body of the let or in the expressions bound to the variables)
preserve' (Let binds bdy _) = preserve' bdy -- TODO: check the binds
preserve' (Ltt binds bdy _) = preserve' bdy
preserve' (Ltr binds bdy _) = preserve' bdy
preserve' (Lrr binds bdy _) = preserve' bdy
-- | PP-CONCAT
preserve' (Bgn [] _) = return True
preserve' (Bgn (e:es) x) = do b <- preserve' e 
                              b' <- preserve' (Bgn es x)
                              return $ b && b'
-- |PP-IF
preserve' (Iff b c a _) = do (p, g) <- get -- TODO: update predicate
                             let pc = p
                             put (pc, g); bc <- preserve' c 
                             let pa = p
                             put (pa, g); ba <- preserve' a
                             put (p, g); return $ bc && ba
-- | PP-APP * (could modify the state in the body of the function)
preserve' (App prc ops _) = return False -- TODO: check if PP holds for the body of the applied function (?)
-- | PP-SKIP (all other expressions don't modify the state and as such are equivalent to skip)
preserve' _ = return True


preserve :: (SchemeAnalysisConstraints var v ctx dep) => (AbstractSto v -> Bool) -> Agreement -> Exp -> Bool -- TODO: do we need the initial predicate here?
preserve p g e = evalState (preserve' e) (p, g)
