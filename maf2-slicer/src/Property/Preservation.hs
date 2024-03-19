{-# LANGUAGE FlexibleContexts #-}

module Property.Preservation(preserve) where 

import Property.Agreement
import Dependency.State
import Syntax.Scheme.AST
import Analysis.Scheme

import qualified Data.Map as Map
import Control.Monad.State

-- | PP(g, e)
preserve' :: (Eq v) => Exp -> State ((AbstractSto v -> Bool), Agreement) Bool --(SchemeAnalysisConstraints var v ctx dep) => Exp -> State ((AbstractSto v -> Bool), Agreement) Bool
-- | PP-ASSIGN
preserve' (Dfv var e _) = do b <- preserve' e 
                             (p, g) <- get
                             let (s, v) = abstractEvalWithPredicate p e
                             -- if this variable is in the agreement, we need to check if the property is preserved by the assignment 
                             let b' = if var `elem` g then v `elem` Map.lookup var s else True 
                             return $ b && b'
preserve' (Dff var args bdy _) = return True -- todo? does a function have an abstract property we can check?
preserve' (Set var e x) = preserve' (Dfv var e x)
-- | PP-LET * (could modify the state in the body of the let or in the expressions bound to the variables)
preserve' (Let binds bdy _) = do b <- preserve' bdy
                                 bs <- sequence $ map (preserve' . snd) binds
                                 return $ b && and bs
preserve' (Ltt binds bdy _) = do b <- preserve' bdy
                                 bs <- sequence $ map (preserve' . snd) binds
                                 return $ b && and bs
preserve' (Ltr binds bdy _) = do b <- preserve' bdy
                                 bs <- sequence $ map (preserve' . snd) binds
                                 return $ b && and bs
preserve' (Lrr binds bdy _) = do b <- preserve' bdy
                                 bs <- sequence $ map (preserve' . snd) binds
                                 return $ b && and bs
-- | PP-CONCAT
preserve' (Bgn [] _) = return True
preserve' (Bgn (e:es) x) = do b <- preserve' e 
                              b' <- preserve' (Bgn es x)
                              return $ b && b'
-- |PP-IF
preserve' (Iff b c a _) = do (p, g) <- get -- IMPROVE: update predicate
                             let pc = p
                             put (pc, g); bc <- preserve' c 
                             let pa = p
                             put (pa, g); ba <- preserve' a
                             put (p, g); return $ bc && ba
-- | PP-APP * (could modify the state in the body of the function)
preserve' (App prc ops _) = return False -- IMPROVE: check if PP holds for the body of the applied function
-- | PP-SKIP (all other expressions don't modify the state and as such are equivalent to skip)
preserve' _ = return True


preserve :: (Eq v) => (AbstractSto v -> Bool) -> Agreement -> Exp -> Bool --(SchemeAnalysisConstraints var v ctx dep) => (AbstractSto v -> Bool) -> Agreement -> Exp -> Bool
preserve p g e = evalState (preserve' e) (p, g)
