module Dependency.Lattice where 

import Domain
import Lattice

class (JoinLattice v) => AtomicLattice v where 
-- | an atom is an element a such that there does not exist another element b with bot < b < a
    atom :: v -> Bool

instance (Ord a, Show a) => AtomicLattice (CP a) where 
    atom (Constant _) = True
    atom _ = False

instance AtomicLattice Sign where 
    atom Zero = True  
    atom Pos = True 
    atom Neg = True 
    atom _ = False  

--- REFINEMENTS

class (TopLattice v) => RefinableLattice v where 
-- | refine returns a list of all elements that are immediate predecessors   
    refine :: v -> [v] 

-- instance (Ord a, Show a, Enum a, Bounded a) => RefinableLattice (CP a) where 
--     refine Bottom = [] -- [Bottom]
--     refine (Constant _) = [Bottom]
--     refine Top = [Constant i | i <- [minBound..]]

instance RefinableLattice Sign where 
    refine SBottom = [] -- [SBottom]
    refine STop = [ZeroOrNeg, ZeroOrPos]    
    refine ZeroOrNeg = [Zero, Neg]
    refine ZeroOrPos = [Zero, Pos]
    refine _ = [SBottom]