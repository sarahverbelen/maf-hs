module Lattice.SignLattice where 

import Lattice.Class 
import Domain.Class

data Sign = SBottom
            | Zero 
            | ZeroOrPos
            | ZeroOrNeg
            | Pos
            | Neg
            | STop 
    deriving (Eq, Ord, Show)

instance Joinable Sign where 
    join SBottom v = v 
    join v SBottom = v 
    join STop _ = STop 
    join _ STop = STop
    join ZeroOrNeg ZeroOrPos = STop
    join ZeroOrPos ZeroOrNeg = STop
    join ZeroOrPos Neg = STop 
    join ZeroOrPos Pos = ZeroOrPos 
    join ZeroOrPos Zero = ZeroOrPos 
    join Neg ZeroOrPos = STop
    join Pos ZeroOrPos = ZeroOrPos
    join Zero ZeroOrPos = ZeroOrPos
    join ZeroOrNeg Neg = ZeroOrNeg 
    join ZeroOrNeg Pos = STop 
    join ZeroOrNeg Zero = ZeroOrNeg 
    join Neg ZeroOrNeg = ZeroOrNeg
    join Pos ZeroOrNeg = STop
    join Zero ZeroOrNeg = ZeroOrNeg
    join Neg Zero = ZeroOrNeg
    join Zero Neg = ZeroOrNeg 
    join Pos Zero = ZeroOrPos 
    join Zero Pos = ZeroOrPos
    join v1 v2 
        | v1 == v2 = v1
    join _ _ = STop

instance Meetable Sign where 
   meet STop v = v
   meet v STop = v
   meet ZeroOrNeg ZeroOrPos = Zero
   meet ZeroOrPos ZeroOrNeg = Zero 
   meet ZeroOrNeg Pos = SBottom
   meet ZeroOrNeg v = v
   meet Pos ZeroOrNeg = SBottom 
   meet v ZeroOrNeg = v 
   meet ZeroOrPos Neg = SBottom
   meet ZeroOrPos v = v
   meet Neg ZeroOrPos = SBottom 
   meet v ZeroOrPos = v 
   meet v1 v2 
        | v1 == v2 = v1
   meet _ _ = SBottom

instance JoinLattice Sign where 
    bottom = SBottom 

    subsumes STop _ = True
    subsumes _ SBottom = True
    subsumes ZeroOrNeg Zero = True 
    subsumes ZeroOrNeg Neg = True 
    subsumes ZeroOrPos Zero = True 
    subsumes ZeroOrPos Pos = True     
    subsumes v1 v2 = v1 == v2
    subsumes _ _ = False

instance (Num c, Ord c) => Domain Sign c where 
    inject x
        | x == 0 = Zero 
        | x < 0 = Neg 
        | x > 0 = Pos   

instance TopLattice Sign where 
    top = STop