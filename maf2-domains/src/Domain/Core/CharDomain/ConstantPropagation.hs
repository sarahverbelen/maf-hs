{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Core.CharDomain.ConstantPropagation where

import Lattice
import Domain.Core.CharDomain.Class
import Domain.Core.BoolDomain.ConstantPropagation

import Data.Char as Char
import Control.Applicative
import Domain.Class
import Domain.Core.NumberDomain.Class

instance CharDomain (CP Char) where
   type IntC (CP Char) = CP Integer
   downcase = return . fmap toLower
   upcase = return . fmap toUpper
   charToInt = return .  fmap (fromIntegral . ord)
   isLower = return . bool . fmap Char.isLower
   isUpper = return . bool . fmap Char.isUpper
   charEq a b = return $ bool $ liftA2 (==) a b
   charLt a b = return $ bool $ liftA2 (<) a b
   charEqCI a b = return $ bool $ liftA2 (==) (toLower <$> a) (toLower <$> b)
   charLtCI a b = return $ bool $ liftA2 (<) (toLower <$> a) (toLower <$> b)

newtype CPChar' i = CPChar' { getCP :: CP Char  }
                  deriving (Eq, Ord, Show,JoinLattice, Joinable)

instance Domain (CPChar' i) Char where
   inject = CPChar' . Constant

instance (IntDomain i) => CharDomain (CPChar' i) where
   type IntC (CPChar' i) = i
   downcase = return . CPChar' . fmap toLower . getCP
   upcase = return . CPChar' . fmap toUpper . getCP
   charToInt c = return $ case getCP c of
      Bottom -> bottom
      Constant a -> inject (fromIntegral $ ord a)
      Top -> intTop
   isLower = return . bool . fmap Char.isLower . getCP
   isUpper = return . bool . fmap Char.isUpper . getCP
   charEq a b = return $ bool $ liftA2 (==) (getCP a) (getCP b)
   charLt a b = return $ bool $ liftA2 (<) (getCP a) (getCP b)
   charEqCI a b = return $ bool $ liftA2 (==) (toLower <$> getCP a) (toLower <$> getCP b)
   charLtCI a b = return $ bool $ liftA2 (<) (toLower <$> getCP a) (toLower <$> getCP b)
