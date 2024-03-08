module Dependency.Atoms where 

import Domain
import Lattice

class (JoinLattice v) => AtomicLattice v where 
-- | an atom is an element a such that there does not exist another element b with bot < b < a
    atom :: v -> Bool

instance (Ord a, Show a) => AtomicLattice (CP a) where 
    atom Bottom = False 
    atom Top = False 
    atom (Constant _) = True

class (JoinLattice v) => RefinableLattice v where 
-- | refine returns a list of all elements that are smaller    
    refine :: v -> [v] 

instance (Ord a, Show a, Enum a, Bounded a) => RefinableLattice (CP a) where 
    refine Bottom = []
    refine (Constant _) = [Bottom]
    refine Top = [Constant i | i <- [minBound..]]