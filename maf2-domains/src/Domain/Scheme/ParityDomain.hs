module Domain.Scheme.ParityDomain(ParityValue) where

import Lattice 
import Domain.Core 
import Domain.Scheme.Class
import Domain.Scheme.Modular 

import Data.Map

type ParityValue ptr var exp = ModularSchemeValue
                                  (CPDouble' Parity)
                                  Parity
                                  (CPChar' Parity)
                                  (CP Bool)
                                  ptr 
                                  ptr 
                                  ptr
                                  var
                                  exp
                                  (Map String var)

type instance VarDom (ParityValue ptr var exp) = ParityValue ptr var exp
type instance PaiDom (ParityValue ptr var exp) = SimplePair (ParityValue ptr var exp)
type instance VecDom (ParityValue ptr var exp) = PIVector (ParityValue ptr var exp) (ParityValue ptr var exp)
type instance StrDom (ParityValue ptr var exp) = SchemeString (CPString' Parity) (ParityValue ptr var exp)
