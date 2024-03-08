module Dependency.Atoms where 

import Lattice

-- an atom is an element a of a uco p such that there does not exist another element b with bot < b < a
class (JoinLattice v) => Atomic v where 
    atom :: v -> Bool
  