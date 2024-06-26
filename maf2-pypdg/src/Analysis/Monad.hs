-- {-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, AllowAmbiguousTypes, PolyKinds #-}
-- {-# LANGUAGE FunctionalDependencies, ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- -- | This module provides monadic operations shared between many analyses.
-- --
-- -- To make instantiations for combinations of these typeclasses easier, this module also provides an abstraction based on Monad **layers**.
-- -- These layers corresponds to monads in a monad transformer stack. However, since `MonadTrans` lacks a method for peeling away a single layer
-- -- from the monad transformer stack, the `MonadLayer` typeclass is used instead. 
-- --
module Analysis.Monad where 
-- module Analysis.Monad(
--  -- Typeclass interfaces
--  StoreM(..),
--  EvalM(..),
--  EnvM(..),
--  AllocM(..),
--  CtxM(..),
--  CallM(..),
--  deref,
--  -- Implementations
--  runSto,
--  runEnv,
--  runCtx,
--  runErr,
--  runErr',
--  runAlloc,
--  runCallBottomT,
--  runNonDetT,
--  -- Types for implementations
--  NonDetT
-- ) where

-- import Analysis.Environment
-- import Control.Monad.Reader hiding (mzero)
-- import Control.Monad.Join
-- import Control.Monad.State hiding (mzero)
-- import Syntax.Scheme.AST
-- import Data.Set (Set)
-- import qualified Data.Set as Set
-- import Data.TypeLevel.List
-- import Data.Kind
-- import Domain
-- import Analysis.Store hiding (lookupSto, extendSto, updateSto)
-- import qualified Analysis.Store as Store
-- import Control.Monad.Layer
-- import Control.Monad.Trans.Maybe
-- import Control.Monad.Writer hiding (mzero)
-- import Control.Monad.Error
-- import List.Transformer
-- import GHC.TypeError
-- import Data.Functor.Identity

-- ----------------------------------------------------------------------------------------------------
-- -- Typeclasses for monadic analysis functionality
-- ----------------------------------------------------------------------------------------------------

-- -- | Reading from an environment 
-- class (Environment env adr) => EnvM m adr env | m -> env, m -> adr where
--    -- | Lookup the address of the given identifier,
--    -- may throw an exception if the identifier is not found 
--    -- since it means that the program is not well-formed.
--    lookupEnv :: String -> m adr
--    -- | Extend the environment with the given list of bindings and run the computation
--    -- given as an argument in it
--    withExtendedEnv :: [(String, adr)] -> m a -> m a
--    -- | Retrieves the current environment 
--    getEnv :: m env
--    -- | Runs function `f` on the environment and to compute the environment to execute `m` in
--    withEnv :: {- f -} (env -> env) -> m a -> m a
--    -- | Runs `m` in the context of the value produced by `(ctx -> ctx)`.

-- -- | Manipulating the context of the current abstract state
-- class CtxM m ctx | m -> ctx where
--    -- | Changes the current context for the given subcomputation
--    withCtx  :: (ctx -> ctx) -> m a -> m a
--    -- | Retrieves the current context
--    getCtx    :: m ctx

-- --
-- -- Store
-- --

-- -- | Lookup
-- lookups :: (JoinLattice a, MonadJoin m) => (adr -> m v) -> (adr -> v -> m a) -> Set adr -> m a
-- lookups look f = Set.fold (mjoin . deref') Control.Monad.Join.mzero
--       where deref' adr = look adr >>= f adr

-- -- 

-- class (Monad m, Address adr) => StoreM m adr where
--    -- | Write to a newly allocated address
--    writeAdr  :: adr -> Vlu adr -> m ()
--    -- | Update an existing address
--    updateAdr :: adr -> Vlu adr -> m ()
--    -- | Lookup the value at the given address, returns bottom if the address does not exist
--    lookupAdr :: adr -> m (Vlu adr)

-- deref :: (StoreM m adr, MonadJoin m, JoinLattice a) => (adr -> Vlu adr -> m a) -> Set adr -> m a
-- deref = lookups lookupAdr

-- --

-- class (Monad m) => AllocM m from (t :: Type -> Type) adr  where
--    alloc :: from -> m adr

-- class (Monad m) => CallM m env v where
--    call :: (Exp, env) -> m v

-- class (Monad m) => EvalM m v e  | m -> v where
--    eval :: e -> m v
--    -- | Runs the evaluation action but return the result of the second argument instead.
--    evalReturn :: e -> m v -> m v
--    evalReturn e m = eval @_ @v e >> m

-- ----------------------------------------------------------------------------------------------------
-- -- Instances
-- ----------------------------------------------------------------------------------------------------

-- newtype EnvT env m a = EnvT { getEnvReader ::  ReaderT env m a } deriving (MonadReader env, Monad, Applicative, MonadLayer, Functor)

-- instance (Monad m, MonadJoin m) => MonadJoin (EnvT env m) where
--    mjoin (EnvT ma) = EnvT . mjoin ma . getEnvReader
--    mzero = EnvT mzero

-- instance {-# OVERLAPPING #-} (Environment env adr, Monad m) => EnvM (EnvT env m) adr env where
--    lookupEnv nam = asks (Analysis.Environment.lookup nam)
--    withExtendedEnv bds = local (extends bds)
--    getEnv = ask
--    withEnv = local

-- instance forall env adr t . (Environment env adr, MonadLayer t, EnvM (Lower t) adr env) => EnvM t adr env where
--    lookupEnv = upperM . lookupEnv
--    withExtendedEnv bds =  lowerM (withExtendedEnv bds)
--    getEnv = upperM getEnv
--    withEnv f = lowerM (withEnv f)

-- runEnv :: forall env m a .  env -> (EnvT env m) a -> m a
-- runEnv initialEnv (EnvT m) = runReaderT m initialEnv

-- ---

-- newtype CtxT ctx m a = CtxT { getContextReader :: ReaderT ctx m a } deriving (MonadReader ctx, Monad, Applicative, MonadLayer, Functor)
-- instance {-# OVERLAPPING #-} Monad m => CtxM (CtxT ctx m) ctx where
--    getCtx = ask
--    withCtx = local
-- instance (MonadLayer t, CtxM (Lower t) ctx) => CtxM t ctx where
--    getCtx =  upperM getCtx
--    withCtx f = lowerM (withCtx f)

-- instance (MonadJoin m) => MonadJoin (CtxT ctx m) where
--    mjoin (CtxT ma) = CtxT . mjoin ma . getContextReader
--    mzero = CtxT mzero

-- runCtx :: ctx -> (CtxT ctx m) a -> m a
-- runCtx initialCtx (CtxT m) = runReaderT m initialCtx

-- ---

-- ---

-- -- | Provides an error capability to the monadic stack. 
-- --
-- -- Errors are recorded by using a writer monad which keeps track of a set of errors,
-- -- whenever an error occurs the analysis halts by returning `Nothing`.
-- --
-- newtype ErrorT m a = ErrorT { runErrorT :: MaybeT (WriterT (Set DomainError) m) a } deriving (Functor, Applicative, Monad)

-- instance (MonadJoin m) => MonadJoin (ErrorT m) where
--    mjoin (ErrorT ma) (ErrorT mb) = ErrorT (mjoin ma mb)
--    mzero = ErrorT mzero
-- instance (Monad m) => MonadLayer (ErrorT m) where
--    type Lower (ErrorT m) = m
--    lowerM f = ErrorT . MaybeT . WriterT . f . runWriterT . runMaybeT . runErrorT
--    upperM = ErrorT . lift . lift

-- instance {-# OVERLAPPING #-} Monad m => MonadError (ErrorT m) where
--    raiseError = ErrorT . lift . tell . Set.singleton >=> (\_ -> ErrorT $ MaybeT $ return Nothing)
-- instance {-# OVERLAPPING #-} (MonadJoin m) => MonadDomainError (ErrorT m) where
--    raiseError = Control.Monad.Error.raiseError
-- instance (MonadJoin (t m), MonadTrans t, MonadError m, Monad m, Monad (t m)) => MonadError (t m) where
--    raiseError = lift . Control.Monad.Error.raiseError
-- instance (MonadJoin (t m), MonadTrans t, MonadError (t m), Monad m, Monad (t m)) => MonadDomainError (t m) where
--    raiseError = Control.Monad.Error.raiseError

-- runErr :: (ErrorT m) a -> m (Maybe a, Set DomainError)
-- runErr = runWriterT . runMaybeT . runErrorT

-- -- Same as `runErr` but ignores the errors
-- runErr' :: Functor m => (ErrorT m) a -> m (Maybe a)
-- runErr' = fmap fst . runErr

-- ---

-- -- | Keeps track of a store using a state monad
-- instance {-# OVERLAPPING #-} (Monad m, Hashable adr, JoinLattice (Vlu adr), Address adr, Typeable adr, Typeable (Vlu adr), Has ks (adr :-> Vlu adr)) => StoreM (StateT (DMap ks) m) adr where
--    writeAdr adr vlu = modify (Store.extendSto adr vlu)
--    updateAdr adr vlu = modify (Store.updateSto adr vlu)
--    lookupAdr = gets  . Store.lookupSto
-- instance (Monad t, Address adr, StoreM (Lower t) adr, MonadLayer t) => StoreM t adr where
--    writeAdr adr =  upperM . writeAdr adr
--    updateAdr adr =  upperM . updateAdr adr
--    lookupAdr  =  upperM .  lookupAdr

-- runSto :: forall ks m a . DMap ks -> (StateT (DMap ks) m) a -> m (a, DMap ks)
-- runSto =  flip runStateT

-- --
-- -- Allocator
-- --

-- -- | Allocator represented as a function
-- type Allocator from ctx to = (from -> ctx -> to)

-- -- Allocator that turns a function into an allocator of the suiteable type
-- newtype AllocT from ctx t to m a = AllocT { getAllocReader :: ReaderT (Allocator from ctx to) m a } deriving (MonadReader (Allocator from ctx to), Monad, Applicative, Functor, MonadLayer)

-- instance (MonadJoin m) => MonadJoin (AllocT from ctx t to m) where
--    mjoin (AllocT ma) = AllocT . mjoin ma . getAllocReader
--    mzero = AllocT mzero

-- instance {-# OVERLAPPING #-} (Monad m, CtxM m ctx) => AllocM (AllocT from ctx t to m) from t to where
--    alloc from = do
--       ctx <- AllocT $ lift getCtx
--       f   <- ask
--       return $ f from ctx

-- instance {-# OVERLAPPING #-} TypeError (Text "No AllocM found on stack") => AllocM Identity from t to

-- instance (Monad m, AllocM (Lower m) from t to, MonadLayer m) => AllocM m from t to where
--    alloc = upperM . alloc @(Lower m) @from @t @to


-- runAlloc :: forall t from ctx to m a . Allocator from ctx to -> AllocT from ctx t to m a ->  m a
-- runAlloc allocator (AllocT m) = runReaderT m allocator


-- -- 
-- -- CallM 
-- -- 

-- instance (Monad m, CallM (Lower m) env v, MonadLayer m) => CallM m env v where
--    call = upperM . call

-- instance {-# OVERLAPPING #-} TypeError (Text "CallM not found on stack") => CallM Identity env v

-- -- | Mock instance that ignores the call and always
-- -- returns bottom.
-- newtype CallBottomT m a = CallBottomT { getCallBottomT :: m a }
--                         deriving (Applicative, Functor, Monad)

-- instance {-# OVERLAPPING #-} (Monad m, JoinLattice v) => CallM (CallBottomT m) env v where
--    call _ = CallBottomT $ return bottom 

-- instance (MonadJoin m) => MonadJoin (CallBottomT m) where
--    mzero = CallBottomT mzero
--    mjoin (CallBottomT ma) = CallBottomT . mjoin ma . getCallBottomT

-- instance (Monad m) => MonadLayer (CallBottomT m) where
--    type Lower (CallBottomT m) = m
--    upperM = CallBottomT
--    lowerM f (CallBottomT m) = CallBottomT $ f m

-- runCallBottomT :: CallBottomT m a -> m a
-- runCallBottomT (CallBottomT ma) = ma

-- -- 
-- -- NonDetT
-- -- 

-- -- | Useful for running the computation non-deterministically 
-- -- and defering join to the end.
-- newtype NonDetT a = NonDetT [a] deriving (Functor, Applicative, Monad)

-- instance MonadJoin NonDetT where
--    mzero = NonDetT []
--    mjoin (NonDetT ma) (NonDetT mb) = NonDetT $ ma ++ mb

-- runNonDetT :: NonDetT a -> [a]
-- runNonDetT (NonDetT a) = a
