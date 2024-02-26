{-# LANGUAGE UndecidableInstances, FlexibleInstances, ConstraintKinds #-}
module Analysis.Scheme where

import Prelude hiding (exp, lookup)

import Analysis.Scheme.Primitives
import qualified Analysis.Scheme.Semantics as Semantics
import Analysis.Scheme.Monad (SchemeM)
import Analysis.Monad hiding (getEnv)

import Control.SVar.ModX
import Control.Monad.Trans.Class
import Control.Monad.Join
import Control.Monad.Layer
import Control.Monad.Cond (whenM)

import Syntax.Scheme
import Lattice
import Domain.Scheme hiding (Exp, Env)

import Data.Set (Set)
import Data.Map (Map)
import Data.Functor.Identity
import Data.TypeLevel.Ghost
import Data.Function ((&))
import Analysis.Monad (EnvM(..))
import Analysis.Scheme.Store
import Control.Monad.DomainError (runMayEscape, DomainError, MonadEscape(..))


import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Analysis.Monad as Monad
import qualified Analysis.Scheme.Monad as SchemeMonad

-----------------------------------------
-- Shorthands
-----------------------------------------

type Program              = Exp
type Env var v ctx dep    = Map String var          -- ^ the initial environment
type Sto var ctx v dep    = Map var v               -- ^ non-heap allocated values
type DSto ctx v           = SchemeStore v
                                       (Adr v)
                                       (SAdr v)
                                       (PAdr v)
                                       (VAdr v)      -- ^ combined store with heap allocated values

-----------------------------------------
-- Store & Environment
-----------------------------------------

class (Ord var) => VarAdr var v ctx dep | var -> v, var -> ctx, var -> dep where
   retAdr :: Component (ModF var v ctx dep) -> var
   prmAdr :: String -> var

-- | The initial environment used by 
-- the analysis
analysisEnvironment :: VarAdr var v ctx dep => Env var v ctx dep
analysisEnvironment = initialEnv prmAdr

-- | The initial store
analysisStore :: forall v ctx dep var . (SchemeAnalysisConstraints var v ctx dep)
              => Env var v ctx dep -> DSto ctx v
analysisStore = fromValues . initialSto @v

-----------------------------------------
-- ModF
-----------------------------------------

data ModF var v ctx dep

class SchemeAlloc ctx var v dep | ctx -> var, ctx -> v, ctx -> dep where
  allocPai :: Exp -> ctx -> PAdr v
  allocVec :: Exp -> ctx -> VAdr v
  allocStr :: Exp -> ctx -> SAdr v
  allocVar :: Ide -> ctx -> var
  allocCtx :: Exp -> ctx -> ctx

instance (SchemeAnalysisConstraints var v ctx dep) => ModX (ModF var v ctx dep) where
  -- A component is a closure + a context
  type Component (ModF var v ctx dep)  = (Exp, Env var v ctx dep, ctx, GT (var, v, dep))
  -- | Global store
  type State (ModF var v ctx dep)      = (DSto ctx v, Map Ide (Set Exp))
  -- | Dependencies are tracked using SVar
  type Dep (ModF var v ctx dep)        = dep
  -- | The analysis of a single component runs the Scheme semantics
  -- on the body of that component
  type MM (ModF var v ctx dep)         = Identity
  analyze (exp, env, ctx, _) (store, t) = 
       let (((_, (spawns, registers, triggers)), sto), t') = (Semantics.eval exp >>= writeAdr (retAdr (exp, env, ctx, Ghost)))
              & runEvalT
              & runMayEscape @_ @(Set DomainError)
              & runCallT @v @ctx
              & runStoreT @VrAdr (values  store)
              & runStoreT @StAdr (strings store)
              & runStoreT @PaAdr (pairs   store)
              & runStoreT @VeAdr (vecs    store)
              & combineStores
              & runAvailableExpressionsT t
              & runEnv env
              & runAlloc @PaAdr (allocPai @ctx)
              & runAlloc @VeAdr (allocVec @ctx)
              & runAlloc @StAdr (allocStr @ctx)
              & runAlloc @VrAdr (allocVar @ctx)
              & runCtx  ctx
              & runIdentity
       in return ((sto, t'), spawns, registers, triggers) 

-----------------------------------------
-- Open recursion for evaluation
-----------------------------------------

newtype BaseSchemeEvalT v m a = BaseSchemeEvalT { getInnerEvalT :: m a } deriving (Monad, Functor, Applicative)

instance (Monad m) => MonadLayer (BaseSchemeEvalT v m) where
   type Lower (BaseSchemeEvalT v m) = m
   upperM = BaseSchemeEvalT 
   lowerM f (BaseSchemeEvalT m) = BaseSchemeEvalT (f m)

-- TODO: this is rather ugly right now but needed
-- since we cannot derive MonadEscape yet if it 
-- is not on top of the layers (see Control.Monad.Layer)
instance (Monad m, MonadEscape m, Esc m ~ Set DomainError) => MonadEscape (BaseSchemeEvalT v m) where
   type Esc (BaseSchemeEvalT v m) = Set DomainError
   escape = upperM . escape
   catch (BaseSchemeEvalT m) hdl = BaseSchemeEvalT $ catch @_ m (getInnerEvalT . hdl)

instance (MonadJoin m) => MonadJoin (BaseSchemeEvalT v m) where
   mzero = BaseSchemeEvalT mzero
   mjoin (BaseSchemeEvalT ma) (BaseSchemeEvalT mb) = BaseSchemeEvalT $ mjoin ma mb

instance MonadTrans (BaseSchemeEvalT v) where
   lift = BaseSchemeEvalT

instance (SchemeM (BaseSchemeEvalT v m) v, AvailableExpressionsM (BaseSchemeEvalT v m), SchemeAnalysisConstraints var v ctx dep) => (Analysis.Monad.EvalM (BaseSchemeEvalT v m) v Exp) where
   eval = availableEval


runEvalT :: BaseSchemeEvalT v m a -> m a
runEvalT (BaseSchemeEvalT m) = m

-----------------------------------------
-- Polymorphic ModF dependencies 
-----------------------------------------

class (Ord dep) => Dependency adr dep | adr -> dep where
   dep :: adr -> dep

-----------------------------------------
-- CallM & StoreM implementation
-----------------------------------------

newtype CallT var v ctx dep m a = CallT (ModxT (ModF var v ctx dep) m a) deriving (Monad, Functor, Applicative, MonadLayer)

instance (Ord (Component (ModF var v ctx dep)), Ord (Dep (ModF var v ctx dep)), MonadJoin m) => MonadJoin (CallT var v ctx dep m) where
   mzero = CallT mzero
   mjoin (CallT ma) (CallT mb) = CallT $ mjoin ma mb

needsUpdate :: forall m adr v t . (JoinLattice v, Show v, StoreM m t adr v) => adr -> v -> m Bool
needsUpdate adr vlu' = do
   vlu <- lookupAdr adr
   if subsumes vlu vlu' then
      return False
   else
      return True

-- | When a store update occurs, registers that update as a write effect,
-- when a store read occurs, registers that read as a read effect
instance {-# OVERLAPPING #-} (
            Dependency adr dep,
            StoreM m t adr v,
            SchemeAnalysisConstraints var v ctx dep
   ) => StoreM (CallT var v ctx dep m) t adr v where

   writeAdr adr vlu = CallT @var @v @ctx $ do
      -- TODO: it is rather strange that type inference
      -- cannot figure out that `c` must equal `ModF v ctx`
      -- hence the explicit type annotations
      whenM (lift $ needsUpdate adr vlu) $ do
         trigger @_ @(ModF var v ctx dep) (dep adr)

      lift $ writeAdr adr vlu

   updateAdr adr vlu = CallT $ do
      whenM (lift $ needsUpdate adr vlu) $ do
         trigger @_ @(ModF var v ctx dep) (dep adr)
      lift $ updateAdr adr vlu

   lookupAdr adr = CallT $ do
      _ <- register @_ @(ModF var v ctx dep) (dep adr)
      lift $ lookupAdr adr


-- | This instances spawns the called function as a component, 
-- and reads the return value from the store.
instance {-# OVERLAPPING #-} (CtxM m ctx,
          Monad m,
          StoreM (CallT var v ctx dep m) t var v, 
          EnvM m var (Env var v ctx dep),
          SchemeAnalysisConstraints var v ctx dep
         ) => CallM (CallT var v ctx dep m) (Env var v ctx dep) v where
   call (Lam _ bdy _, _) = do
      -- get the extended environment 
      env' <- CallT $ lift getEnv
      -- get the current context
      ctx <- CallT $ lift getCtx
      -- create a new component from this context
      let comp = (bdy, env', ctx, Ghost)
      --  spawn  the new component
      _ <- CallT $ spawn comp
      -- lookup the return value of the component
      lookupAdr (retAdr comp)

-- | Run the CallT monad and peel it off the stack whilst returning 
-- its encapsulated ModX state.
runCallT :: forall v ctx m a c dep var . (Monad m, c ~ ModF var v ctx dep)
         => CallT var v ctx dep m a
         -> m (a, ([Component c], [Dep c], [Dep c]))
runCallT (CallT m) = runModxT @c m

-----------------------------------------
-- Available Expressions analysis
-----------------------------------------

class AvailableExpressionsM m where
   addAvailable :: Ide -> Exp -> m ()

availableEval :: forall m v . (AvailableExpressionsM m, SchemeDomain v, SchemeM m v) => Exp -> m v
availableEval (Let bds bdy _) = do
   vlus <- mapM Monad.eval eps
   mapM_ (uncurry addAvailable) bds
   adrs <- mapM SchemeMonad.allocVar vrs
   mapM_ (uncurry writeAdr) (zip adrs vlus)
   withExtendedEnv (zip (map name vrs) adrs) (Monad.eval bdy)
   where (vrs, eps) = unzip bds
availableEval e = Semantics.eval e

newtype AvailableExpressionsT m v = AvailableExpressionsT (State.StateT (Map Ide (Set Exp)) m v) 
                                    deriving (Monad, Applicative, Functor, MonadLayer, State.MonadState (Map Ide (Set Exp)))

instance {-# OVERLAPPING #-} (Monad m) => AvailableExpressionsM (AvailableExpressionsT m) where
   addAvailable ide exp = State.modify (Map.insert ide (Set.singleton exp))

instance (MonadLayer m, AvailableExpressionsM (Lower m)) => AvailableExpressionsM m where 
   addAvailable ide exp = upperM $ addAvailable ide exp 

instance (MonadJoin m) => MonadJoin (AvailableExpressionsT m) where
   mzero = AvailableExpressionsT mzero
   mjoin (AvailableExpressionsT ma) (AvailableExpressionsT mb) = AvailableExpressionsT $ mjoin ma mb

runAvailableExpressionsT :: Map Ide (Set Exp) -> AvailableExpressionsT m a -> m (a, Map Ide (Set Exp))
runAvailableExpressionsT t (AvailableExpressionsT m) = State.runStateT m t  

-----------------------------------------
-- Analysis
-----------------------------------------

-- TODO: too many constraints, makes it more difficult to parse the program
-- as written, try to simplify
type SchemeAnalysisConstraints var v ctx dep = (
         Show v,
         SchemeValue v,
         Adr v ~ var,
         SchemeConstraints v Exp var (Env var v ctx dep),
         Dependency var dep,
         Dependency (PAdr v) dep,
         Dependency (SAdr v) dep,
         Dependency (VAdr v) dep,
         VarAdr var v ctx dep,
         Ord ctx, Ord v, SchemeAlloc ctx var v dep)

-- | The result of the analysis
newtype AnalysisResult var v ctx dep = AnalysisResult (State (ModF var v ctx dep))

-- | Analyses the given program into an analysis
-- result. It uses the default initial environment
-- as specified in `Analysis.Scheme.Primitives`
analyzeProgram :: forall v ctx wl dep var .
                  (WorkList wl (Component (ModF var v ctx dep)), SchemeAnalysisConstraints var v ctx dep)
               => Program  -- ^ the program analyse
               -> wl       -- ^ the initial contents of the worklist, can be empty. This function will add the initial component to it. 
               -> ctx -- ^ context allocation function for a given expression (usually associated with a function call)
               -> State (ModF var v ctx dep)
analyzeProgram exp initialWl initialCtx = runIdentity $ runModX initialWl' (analysisStore @v analysisEnvironment, Map.empty)
  where initialWl' = add (exp, analysisEnvironment, initialCtx, Ghost) initialWl
