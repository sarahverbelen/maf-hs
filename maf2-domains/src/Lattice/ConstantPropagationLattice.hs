{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Lattice.ConstantPropagationLattice(CP(..), fromCP) where

import Lattice.Class 
import Domain.Class 

import GHC.Generics

data CP a = Bottom 
          | Constant a 
          | Top
    deriving (Eq, Ord, Show, Generic)

instance Ord a => Joinable (CP a) where
    join Bottom v = v
    join v Bottom = v
    join v@(Constant x1) (Constant x2)
      | x1 == x2 = v
    join _ _ = Top

instance Ord a => Meetable (CP a) where 
   meet Top v = v
   meet v Top = v
   meet v@(Constant x1) (Constant x2)
      | x1 == x2 = v
   meet _ _ = Bottom

instance (Show a, Ord a) => JoinLattice (CP a) where
    bottom = Bottom

    subsumes Top _ = True
    subsumes _ Bottom = True
    subsumes (Constant x1) (Constant x2) = x1 == x2
    subsumes _ _ = False

instance (Show a, Ord a) => Domain (CP a) a where
   inject = Constant

instance Functor CP where
    fmap _ Bottom = Bottom
    fmap f (Constant x) = Constant (f x)
    fmap _ Top = Top

instance Applicative CP where
    pure = Constant
    Bottom <*> _ = Bottom
    _ <*> Bottom = Bottom
    (Constant f) <*> (Constant a) = Constant (f a)
    _ <*> _ = Top

instance (Ord a, Show a) => TopLattice (CP a) where 
    top = Top    

-- | Convert from a CP lattice to another abstract domain
fromCP :: (TopLattice a, Domain a c) => CP c -> a
fromCP Bottom       = bottom
fromCP (Constant c) = inject c
fromCP Top          = top