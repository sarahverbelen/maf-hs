{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Core.NumberDomain.ConstantPropagation where

import Lattice 
import Domain.Core.NumberDomain.Class 
import Domain.Core.BoolDomain.ConstantPropagation () -- for CP Bool instance
import Domain.Class


import Control.Applicative
import Control.Monad.DomainError
import Control.Monad.Join 
import Control.Monad.AbstractM

------------------------------------------------------------
--- Integers
------------------------------------------------------------

instance NumberDomain (CP Integer) where
   type Boo (CP Integer) = CP Bool
   isZero = return . liftA2 (==) (Constant 0)
   random = return . const Top
   plus a b = return $ liftA2 (+) a b
   minus a b = return $ liftA2 (-) a b
   times a b = return $ liftA2 (*) a b
   div a b = return $ liftA2 Prelude.div a b
   expt a b = return $ liftA2 (^) a b
   lt a b = return $ liftA2 (<) a b
   eq a b = return $ liftA2 (==) a b

instance IntDomain (CP Integer) where
   type Str (CP Integer) = CP String
   type Rea (CP Integer) = CP Double
   toReal   = return . fmap fromIntegral
   toString = return . fmap show
   quotient a b = return $ liftA2 Prelude.div a b
   remainder a b =  return $ liftA2 rem a b
   modulo a b = return $ liftA2 mod a b


------------------------------------------------------------
--- Reals
------------------------------------------------------------

between :: Ord a => a -> a -> a -> Bool
between a b c = a <= c && c <= b

instance NumberDomain (CP Double) where
   type Boo (CP Double) = CP Bool
   isZero = return . liftA2 (==) (Constant 0)
   random = return . const Top
   plus a b = return $ liftA2 (+) a b
   minus a b = return $ liftA2 (-) a b
   times a b = return $ liftA2 (*) a b
   div a b = return $ liftA2 (/) a b
   expt a b = return $ liftA2 (**) a b
   lt a b = return $ liftA2 (<) a b
   eq a b = return $ liftA2 (==) a b

instance RealDomain (CP Double) where
   type IntR (CP Double) = CP Integer
   toInt = return . fmap truncate
   ceiling = return . fmap (fromIntegral . Prelude.ceiling)
   floor = return . fmap (fromIntegral . Prelude.floor)
   round = return . fmap (fromIntegral . Prelude.round)
   log a =
      domain (fmap (<= 0) a) InvalidArgument <||> return (fmap Prelude.log a)
   sin = return . fmap Prelude.sin
   asin a =
      domain (fmap (between (-1) 1) a) InvalidArgument <||>
      return (fmap Prelude.asin a)
   cos  = return . fmap Prelude.cos
   acos a =
      domain (fmap (between (-1) 1) a) InvalidArgument <||>
      return (fmap Prelude.acos a)
   tan = return . fmap Prelude.tan
   atan = return . fmap Prelude.atan
   sqrt a =
      domain (fmap (< 0) a) InvalidArgument <||>
      return (fmap Prelude.sqrt a)

newtype CPDouble' i = CPDouble' { getCPD :: CP Double }
                        deriving (Eq, Ord, Show, JoinLattice, Joinable)

instance Domain (CPDouble' i) Double where 
   inject = CPDouble' . Constant                        

instance NumberDomain (CPDouble' i) where
   type Boo (CPDouble' i) = CP Bool
   isZero a = return (liftA2 (==) (Constant 0) (getCPD a))
   random _ = return $ CPDouble' Top
   plus (CPDouble' a) (CPDouble' b) = return $ CPDouble' (liftA2 (+) a b)
   minus (CPDouble' a) (CPDouble' b) = return $ CPDouble' (liftA2 (-) a b)
   times (CPDouble' a) (CPDouble' b) = return $ CPDouble' (liftA2 (*) a b)
   div (CPDouble' a) (CPDouble' b) = return $ CPDouble' (liftA2 (/) a b)
   expt (CPDouble' a) (CPDouble' b) = return $ CPDouble' (liftA2 (**) a b)
   lt (CPDouble' a) (CPDouble' b) = return $ liftA2 (<) a b 
   eq (CPDouble' a) (CPDouble' b) = return $ liftA2 (==) a b

instance (IntDomain i) => RealDomain (CPDouble' i) where
   type IntR (CPDouble' i) = i
   toInt _ = return intTop
   ceiling a = return $ CPDouble' (fmap (fromIntegral . Prelude.ceiling) (getCPD a))
   floor a = return $ CPDouble' (fmap (fromIntegral . Prelude.floor) (getCPD a))
   round a = return $ CPDouble' (fmap (fromIntegral . Prelude.round) (getCPD a))
   log a =
      domain (fmap (<= 0) (getCPD a)) InvalidArgument <||> return (CPDouble' (fmap Prelude.log (getCPD a)))
   sin a = return $ CPDouble' (fmap Prelude.sin (getCPD a))
   asin a =
      domain (fmap (between (-1) 1) (getCPD a)) InvalidArgument <||>
      return (CPDouble' (fmap Prelude.asin (getCPD a)))
   cos a = return $ CPDouble' (fmap Prelude.cos (getCPD a))
   acos a =
      domain (fmap (between (-1) 1) (getCPD a)) InvalidArgument <||>
      return (CPDouble' (fmap Prelude.acos (getCPD a)))
   tan a = return $ CPDouble' (fmap Prelude.tan (getCPD a))
   atan a = return $ CPDouble' (fmap Prelude.atan (getCPD a))
   sqrt a =
      domain (fmap (< 0) (getCPD a)) InvalidArgument <||>
      return (CPDouble' (fmap Prelude.sqrt (getCPD a)))     

instance TopLattice (CPDouble' i) where 
   top = CPDouble' Top      