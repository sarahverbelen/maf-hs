{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Core.NumberDomain.Parity where

import Lattice 
import Domain.Core.NumberDomain.Class 
import Domain.Core.NumberDomain.ConstantPropagation

import Control.Monad.DomainError
import Control.Monad.Join 
import Control.Monad.AbstractM

import Prelude hiding (div)

instance NumberDomain Parity where
    type Boo Parity = CP Bool
    isZero v = return Top
    random _ = return PTop

    plus PBottom _ = return PBottom
    plus _ PBottom = return PBottom
    plus Even Even = return Even 
    plus Odd Odd = return Even 
    plus Odd Even = return Odd 
    plus Even Odd = return Odd
    plus _ _ = return PTop

    minus = plus

    times Even _ = return Even 
    times _ Even = return Even
    times PBottom _ = return PBottom 
    times _ PBottom = return PBottom 
    times Odd Odd = return Odd 
    times _ _ = return PTop

    div PBottom _ = return PBottom 
    div _ PBottom = return PBottom 
    div _ _ = return PTop

    expt PBottom _ = return PBottom
    expt _ PBottom = return PBottom
    expt Even _ = return Even 
    expt _ _ = return PTop

    lt _ _ = return $ Top

    eq _ _ = return Top

instance IntDomain Parity where
    type Str Parity = String
    type Rea Parity = CPDouble' Parity
    toReal _ = return $ CPDouble' Top
    toString v = return $ (show v)
   
    quotient = div 

    remainder _ _ =  return PTop
    modulo _ _ = return PTop