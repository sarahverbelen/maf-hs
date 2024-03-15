module Property.Preservation where 

import Property.Agreement
import Dependency.State
import Syntax.Scheme.AST

import qualified Data.Map as Map
import Control.Monad.State

type PPState v = (AbstractSto Ide v, Agreement)

-- | PP(g, e)  TODO: add the predicate B
preserve' :: (Eq v) => Exp -> State (PPState v) Bool
-- PP-ASSIGN
preserve' (Dfv var e _) = do b <- preserve' e 
                             let v = undefined -- the value of the expression in the current state
                             (s, g) <- get
                             let b' = v `elem` Map.lookup var s -- check if the property is the same as before the assignment (if the var existed then)
                             put (Map.insert var v s, g) -- update the state to include this new binding for var
                             return $ b && b'
-- preserve' (Dff var ps e _) = TODO
preserve' (Set var e x) = preserve' (Dfv var e x)
-- -- | PP-LET *
-- preserve' (Let binds e _) = undefined
-- preserve' (Ltt binds e _) = undefined
-- preserve' (Ltr binds e _) = undefined
-- preserve' (Lrr binds e _) = undefined
-- | PP-CONCAT
preserve' (Bgn [] _) = return True
preserve' (Bgn (e:es) x) = do b <- preserve' e 
                              b' <- preserve' (Bgn es x)
                              return $ b && b'
-- |PP-IF
preserve' (Iff b c a _) = do bc <- preserve' c 
                             ba <- preserve' a 
                             return $ bc && ba
-- | PP-APP *
preserve' (App prc ops _) = return False -- TODO: check if PP holds for the body of the applied function
-- | PP-SKIP
preserve' _ = return True


preserve :: (Eq v) => AbstractSto Ide v -> Agreement -> Exp -> Bool
preserve s g e = evalState (preserve' e) (s, g)
