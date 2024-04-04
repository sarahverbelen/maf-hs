{-# LANGUAGE ScopedTypeVariables #-}

module Property.Preservation(preserve, preserveWithSto) where 

import Property.Agreement
import Dependency.State
import Dependency.Lattice

import Lattice
import Syntax.Scheme.AST
import Control.Monad.State

import qualified Data.Map as Map

type PreserveState = State (AbstractSto V) Bool

---------------------------------------------------------------------------------------------------
--- PP-SYSTEM RULES
---------------------------------------------------------------------------------------------------

-- | PP(g, e)
preserve :: AbstractSto V -> Agreement -> Exp -> Bool
preserve s g e = fst $ preserveWithSto s g e

preserveWithSto :: AbstractSto V -> Agreement -> Exp -> (Bool, AbstractSto V)
preserveWithSto s g e = runState (preserve' e g) s

preserve' :: Exp -> Agreement -> PreserveState
-- | PP-ASSIGN
preserve' (Dfv var e _) g       = preserveBinding g (var, e)
preserve' (Set var e _) g       = preserveBinding g (var, e)
-- | PP-LET
preserve' (Let binds bdy _) g   = preserveLet binds bdy g
preserve' (Ltt binds bdy _) g   = preserveLet binds bdy g
preserve' (Ltr binds bdy _) g   = preserveLet binds bdy g
preserve' (Lrr binds bdy _) g   = preserveLet binds bdy g
-- | PP-CONCAT
preserve' (Bgn [] _) _ = return True
preserve' (Bgn (e:es) x) g      = do    b <- preserve' e g 
                                        b' <- preserve' (Bgn es x) g
                                        return $ b && b'
-- |PP-IF
preserve' (Iff b c a _) g       = preserveIf b c a g
-- | PP-APP *
preserve' (App _ _ _) _         = return False -- TODO
-- | PP-FUNCTIONDEF *
preserve' (Dff _ _ _ _) _       = return False -- TODO
-- | PP-SKIP (all other expressions don't modify the state and as such are equivalent to skip)
preserve' _ _                   = return True


-- | PP-LET
preserveLet :: [(Ide, Exp)] -> Exp -> Agreement -> PreserveState
preserveLet binds bdy g = do  bs <- sequence $ map (preserveBinding g) binds
                              b <- preserve' bdy g
                              return $ b && and bs

-- | PP-ASSIGN              
preserveBinding :: Agreement -> (Ide, Exp) -> PreserveState             
preserveBinding g (var, e) = do --b <- preserve' e g -- TODO: reinclude when function applications are dealt with (body of function could modify the state)
                                let b = True
                                let mkIde i = Ide (name i) NoSpan -- assume variables are unique by name.. (because different Ide =/= different variable..)
                                s <- get
                                let v = abstractEvalForCovering e s
                                -- update the value in our abstract state
                                let s' = Map.insert (mkIde var) v s
                                put s'
                                -- if this variable is in the agreement, we need to check if the property is preserved by the assignment     
                                -- if the variable wasn't defined yet and it is in the agreement, the property is not preserved (ensures we don't remove the first definition of necessary variables!)
                                let b' = (not ((name var) `elem` g)) || ((v /= top) && ((Map.lookup (mkIde var) s) /= Nothing) && (v `elem` Map.lookup (mkIde var) s))
                                return $ b && b'

-- |PP-IF (assumes no side effects in condition)
preserveIf :: Exp -> Exp -> Exp -> Agreement -> PreserveState
preserveIf _ c a g = do   s <- get 
                          let sc = s -- todo: update with info from condition
                          put sc; bc <- preserve' c g 
                          let sa = s -- todo: update with info from condition
                          put sa; ba <- preserve' a g
                          put s; return $ bc && ba                             