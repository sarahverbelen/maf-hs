{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Core.StringDomain.ConstantPropagation(CPString') where

import Lattice
import Domain.Core.StringDomain.Class
import Domain.Core.BoolDomain.ConstantPropagation () -- for (CP Bool instance)
import Control.Monad.AbstractM
import Control.Monad.DomainError
import Control.Monad.Join

import Control.Applicative
import Domain.Core.CharDomain.ConstantPropagation (CPChar')
import Domain.Class (Domain(..))

-- TODO: move to some util package 
replaceAt :: [a] -> Integer -> a -> [a]
replaceAt (_:xs) 0 c = c : xs
replaceAt (x:xs) n c = x : replaceAt xs (n-1) c
replaceAt [] _ _ = []

instance StringDomain (CP String) where
   type IntS (CP String) = CP Integer
   type ChaS (CP String) = CP Char
   type BooS (CP String) = CP Bool

   length = return . fmap (fromIntegral . Prelude.length)
   append a b = return $ liftA2 (++) a b
   ref s i =
      domain (liftA2 (>) (fmap fromIntegral i) (fmap Prelude.length s)) IndexOutOfBounds <||>
      return (liftA2 (!!) s (fmap fromIntegral i))
   set s i c =
      domain (liftA2 (>) (fmap fromIntegral i) (fmap Prelude.length s)) IndexOutOfBounds <||>
      return (replaceAt <$> s <*> i <*> c)
   stringLt s1 s2 = return $ liftA2 (<) s1 s2
   toNumber = return . fmap read
   makeString i c = return $ take <$> fmap fromIntegral i <*> fmap repeat c


newtype CPString' i = CPString' { getCP :: CP String  }
                    deriving (JoinLattice, Joinable, Eq, Ord, Show)
instance Domain (CPString' i) String where   
   inject = CPString' . inject
instance StringDomain (CPString' i) where
   type IntS (CPString' i) = i
   type ChaS (CPString' i) = CPChar' i
   type BooS (CPString' i) = CP Bool

   length = undefined -- return . fmap (fromIntegral . Prelude.length)
   append a b = return $ CPString' $ liftA2 (++) (getCP a) (getCP b)
   ref s i = undefined
      --domain (liftA2 (>) (fmap fromIntegral i) (fmap Prelude.length s)) IndexOutOfBounds <||>
      --return (liftA2 (!!) s (fmap fromIntegral i))
   set s i c = undefined
      --domain (liftA2 (>) (fmap fromIntegral i) (fmap Prelude.length s)) IndexOutOfBounds <||>
      --return (replaceAt <$> s <*> i <*> c)
   stringLt s1 s2 = return $ liftA2 (<) (getCP s1) (getCP s2)
   toNumber = undefined -- return . fmap read
   makeString i c = undefined -- return $ take <$> fmap fromIntegral i <*> fmap repeat c
