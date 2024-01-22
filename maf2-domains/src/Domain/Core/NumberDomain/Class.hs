module Domain.Core.NumberDomain.Class (NumberDomain(..), IntDomain(..), RealDomain(..)) where

import Lattice
import Domain.Class 
import Control.Monad.AbstractM

import Data.Kind 

class NumberDomain n where
   type Boo n :: Type
   isZero :: AbstractM m => n -> m (Boo n)
   random :: AbstractM m => n -> m n
   plus :: AbstractM m => n -> n -> m n
   minus :: AbstractM m => n -> n -> m n
   times :: AbstractM m => n -> n -> m n
   div :: AbstractM m => n -> n -> m n
   expt :: AbstractM m => n -> n -> m n
   lt :: AbstractM m => n -> n -> m (Boo n)
   equals :: AbstractM m => n -> n -> m (Boo n)

class (Domain i Integer, NumberDomain i) => IntDomain i where
   type Str i :: Type
   type Rea i :: Type
   toReal :: AbstractM m => i -> m (Rea i)
   toString :: AbstractM m => i -> m (Str i )
   quotient :: AbstractM m => i -> i -> m i
   modulo :: AbstractM m => i -> i -> m i
   remainder :: AbstractM m => i -> i -> m i

class (Domain r Double, NumberDomain r) => RealDomain r where
   type IntR r :: Type
   toInt :: AbstractM m => r -> m (IntR r)
   ceiling :: AbstractM m => r -> m r
   floor :: AbstractM m => r -> m r
   round :: AbstractM m => r -> m r
   log :: AbstractM m => r -> m r
   sin :: AbstractM m => r -> m r
   asin :: AbstractM m => r -> m r
   cos :: AbstractM m => r -> m r
   acos :: AbstractM m => r -> m r
   tan :: AbstractM m => r -> m r
   atan :: AbstractM m => r -> m r
   sqrt :: AbstractM m => r -> m r