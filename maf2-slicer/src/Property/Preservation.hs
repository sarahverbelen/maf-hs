{-# LANGUAGE ScopedTypeVariables #-}

module Property.Preservation(preserve) where 

import Property.Agreement
import Dependency.State
import Dependency.Lattice
import Syntax.Scheme.AST
import Analysis.Scheme

import qualified Data.Map as Map
import Control.Monad.State

type PreserveState v = State (AbstractSto v, Agreement) Bool

-- | PP(g, e)
preserve :: (RefinableLattice v) => AbstractSto v -> Agreement -> Exp -> Bool
preserve p g e = evalState (preserve' e) (p, g)


preserve' :: forall v . (RefinableLattice v) => Exp -> PreserveState v
-- | PP-ASSIGN
preserve' (Dfv var e _) = preserveBinding (var, e)
preserve' (Dff var args bdy _) = return True -- todo?
preserve' (Set var e x) = preserveBinding (var, e)
-- | PP-LET
preserve' (Let binds bdy _) = preserveLet binds bdy
preserve' (Ltt binds bdy _) = preserveLet binds bdy
preserve' (Ltr binds bdy _) = preserveLet binds bdy
preserve' (Lrr binds bdy _) = preserveLet binds bdy
-- | PP-CONCAT
preserve' (Bgn [] _) = return True
preserve' (Bgn (e:es) x) = do b <- preserve' e 
                              b' <- preserve' (Bgn es x)
                              return $ b && b'
-- |PP-IF
preserve' (Iff b c a _) = preserveIf b c a
-- | PP-APP * (TODO)
preserve' (App prc ops _) = return False
-- | PP-SKIP (all other expressions don't modify the state and as such are equivalent to skip)
preserve' _ = return True


-- | PP-LET
preserveLet :: forall v . (RefinableLattice v) => [(Ide, Exp)] -> Exp -> PreserveState v
preserveLet binds bdy = do  bs <- sequence $ map preserveBinding binds
                            b <- preserve' bdy 
                            return $ b && and bs

-- | PP-ASSIGN              
preserveBinding :: forall v . (RefinableLattice v) => (Ide, Exp) -> PreserveState v              
preserveBinding (var, e) = do   b <- preserve' e 
                                (s, g) <- get
                                let v = abstractEvalWithState s e
                                -- update the value in our abstract state
                                let s' = Map.insert var v s
                                put (s', g)
                                -- if this variable is in the agreement, we need to check if the property is preserved by the assignment 
                                let ideEq = (\a b -> name a == name b)    
                                let b' = if any (ideEq var) g then v `elem` Map.lookup var s else True 
                                return $ b && b'

-- |PP-IF (assumes no side effects in condition)
preserveIf :: forall v . (RefinableLattice v) => Exp -> Exp -> Exp -> PreserveState v
preserveIf _ c a = do   (s, g) <- get 
                        let sc = s -- todo: update with info from condition
                        put (sc, g); bc <- preserve' c 
                        let sa = s -- todo: update with info from condition
                        put (sa, g); ba <- preserve' a
                        put (s, g); return $ bc && ba                             