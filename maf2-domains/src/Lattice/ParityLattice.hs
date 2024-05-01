module Lattice.ParityLattice where 

import Lattice.Class 
import Domain.Class

data Parity = PBottom
            | Even
            | Odd
            | PTop 
    deriving (Eq, Ord, Show)

instance Joinable Parity where 
    join PBottom v = v 
    join v PBottom = v 
    join PTop _ = PTop 
    join _ PTop = PTop
    join v1 v2 
        | v1 == v2 = v1
    join _ _ = PTop

instance Meetable Parity where 
   meet PTop v = v
   meet v PTop = v
   meet v1 v2 
        | v1 == v2 = v1
   meet _ _ = PBottom

instance JoinLattice Parity where 
    bottom = PBottom 

    subsumes PTop _ = True
    subsumes _ PBottom = True 
    subsumes v1 v2 = v1 == v2
    subsumes _ _ = False

instance (Integral c) => Domain Parity c where 
    inject x
        | even x = Even 
        | odd x = Odd  

instance TopLattice Parity where 
    top = PTop