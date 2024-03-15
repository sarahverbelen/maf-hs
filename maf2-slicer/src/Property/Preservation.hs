module Property.Preservation where 

import Property.Agreement
import Dependency.State
import Syntax.Scheme.AST

import qualified Data.Map as Map
import Control.Monad.State

type PPState v = (AbstractSto Ide v, Agreement)

-- | PP(g, e)  TODO: add the predicate B (?)
preserve' :: (Eq v) => Exp -> State (PPState v) Bool
-- | PP-ASSIGN
preserve' (Dfv var e _) = do b <- preserve' e 
                             let v = undefined -- the value of the expression in the current state
                             (s, g) <- get
                             let b' = v `elem` Map.lookup var s -- check if the property is the same as before the assignment (if the var existed then)
                             put (Map.insert var v s, g) -- update the state to include this new binding for var
                             return $ b && b'
-- preserve' (Dff var args bdy _) = TODO
preserve' (Set var e x) = preserve' (Dfv var e x)
-- | PP-LET * (could modify the state in the body of the let)
preserve' (Let binds bdy _) = preserve' bdy -- TODO: add binds to the state
preserve' (Ltt binds bdy _) = preserve' bdy -- TODO: add binds to the state
preserve' (Ltr binds bdy _) = preserve' bdy -- TODO: add binds to the state
preserve' (Lrr binds bdy _) = preserve' bdy -- TODO: add binds to the state
-- | PP-CONCAT
preserve' (Bgn [] _) = return True
preserve' (Bgn (e:es) x) = do b <- preserve' e 
                              b' <- preserve' (Bgn es x)
                              return $ b && b'
-- |PP-IF
preserve' (Iff b c a _) = do (s, g) <- get; bc <- preserve' c 
                             put (s, g); ba <- preserve' a -- we need to ensure that both branches are checked for the same state
                             return $ bc && ba
-- | PP-APP * (could modify the state in the body of the function)
preserve' (App prc ops _) = return False -- TODO: check if PP holds for the body of the applied function
-- | PP-SKIP (all other expressions don't modify the state and as such are equivalent to skip)
preserve' _ = return True


preserve :: (Eq v) => AbstractSto Ide v -> Agreement -> Exp -> Bool
preserve s g e = evalState (preserve' e) (s, g)
