{-# LANGUAGE FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Dependency.Lattice where 

import Domain hiding (Exp)
import Lattice
import Syntax.Scheme.AST
import qualified Control.Monad.Escape as Escape

import Prelude hiding (null)

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


instance (AtomicLattice a) => AtomicLattice (Maybe a) where 
    atom (Just x) = atom x
    atom Nothing = False

instance (AtomicLattice a) => AtomicLattice (CPChar' a) where 
    atom (CPChar' x) = atom x         

class (TopLattice v) => RefinableLattice v where 
-- | refine returns a list of all elements that are immediate predecessors   
    refine :: v -> [v] 

instance (Ord a, Show a, Enum a, Bounded a) => RefinableLattice (CP a) where 
    refine Bottom = []
    refine (Constant _) = [Bottom]
    refine Top = [Constant i | i <- [minBound..]]

instance RefinableLattice Sign where 
    refine SBottom = []
    refine STop = [ZeroOrNeg, ZeroOrPos]    
    refine ZeroOrNeg = [Zero, Neg]
    refine ZeroOrPos = [Zero, Pos]
    refine _ = [SBottom]

instance (RefinableLattice a) => RefinableLattice (Maybe a) where 
    refine (Just x) = [Just a | a <- refine x]
    refine Nothing = []  

instance (RefinableLattice a) => RefinableLattice (CPChar' a) where 
    refine (CPChar' x) = [CPChar' a | a <- refine x]
 
instance (TopLattice a) => TopLattice (Maybe a) where 
    top = Just top 

type V = SignValue () Ide Exp

instance Address Ide
instance Address ()

instance (TopLattice (ModularSchemeValue r i c b pai vec str var exp env), RefinableLattice r, RefinableLattice i, RefinableLattice c, RefinableLattice b)
    => RefinableLattice (ModularSchemeValue r i c b pai vec str var exp env) where
        refine v = [ModularSchemeValue {
                real = r,
                integer = i,
                character = character v,
                boolean = b,
                paiPtr = paiPtr v,
                vecPtr = vecPtr v,
                strPtr = strPtr v,
                clo = clo v,
                null = null v,
                unspecified = unspecified v,
                primitives = primitives v
        } | r <- refine $ real v, i <- refine $ integer v, b <- refine $ boolean v]

instance (AtomicLattice r, AtomicLattice i, AtomicLattice b, RealDomain r, IntDomain i, CharDomain c, BoolDomain b, Address pai, Address vec, Address str, Ord env, Ord exp, Show env)
    => AtomicLattice (ModularSchemeValue r i c b pai vec str var exp env) where
        atom v = (atom $ real v) && (atom $ integer v) && (atom $ boolean v)


instance (AtomicLattice a, Joinable e, Eq e) => AtomicLattice (Escape.MayEscape e a) where 
    atom Escape.Bottom = False 
    atom (Escape.Escape _) = False 
    atom (Escape.Value v) = atom v 
    atom (Escape.MayBoth v _) = atom v        

instance (TopLattice a, Joinable e, Eq e) => TopLattice (Escape.MayEscape e a) where 
    top = Escape.Value top    

class DummyValue v c where 
-- | an abstract value that can be "unabstracted" into an example concrete value
    dummyValue :: v -> c

instance (Num i) => DummyValue Sign i where 
    dummyValue ZeroOrNeg = -1
    dummyValue ZeroOrPos = 1 
    dummyValue Zero = 0 
    dummyValue Pos = 2 
    dummyValue Neg = -2
    dummyValue _ = 3


instance (DummyValue a c) => DummyValue (CP a) c where 
    dummyValue (Constant v) = dummyValue v 
    --dummyValue _ = 

instance DummyValue Bool Bool where 
    dummyValue b = b

instance (DummyValue a c) => DummyValue (Maybe a) c where 
    dummyValue (Just v) = dummyValue v
    dummyValue Nothing = error "nothing value for dummy"


instance (DummyValue r Double, DummyValue i Integer, DummyValue b Bool, RealDomain r, Ord i, Ord r, Ord c, Ord b, IntDomain i, CharDomain c, BoolDomain b, Address pai, Address vec, Address str, Ord env, Ord exp, Show env, Show (ModularSchemeValue r i c b pai vec str var exp env)) 
    => DummyValue (ModularSchemeValue r i c b pai vec str var exp env) Exp where
    dummyValue v = 
        if (real v) /= Nothing
            then Rea (dummyValue $ real v) NoSpan 
            else if (integer v) /= Nothing 
                    then Num (dummyValue $ integer v) NoSpan 
                    else if (boolean v) /= Nothing 
                        then Bln (dummyValue $ boolean v) NoSpan 
                        else Num 6 NoSpan