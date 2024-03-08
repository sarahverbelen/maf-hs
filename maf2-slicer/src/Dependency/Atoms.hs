module Dependency.Atoms where 

import Lattice

-- an atom is an element a of a uco p such that there does not exist another element b with bot < b < a
class (JoinLattice v) => AtomicLattice v where 
    atom :: v -> Bool


instance (Ord a, Show a) => AtomicLattice (CP a) where 
    atom Bottom = False 
    atom Top = False 
    atom (Constant _) = True