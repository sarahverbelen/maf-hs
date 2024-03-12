module Domain.Scheme.SignDomain(SignValue) where

import Lattice 
import Domain.Core 
import Domain.Scheme.Class
import Domain.Scheme.Modular 

import Data.Map

type SignValue ptr var exp = ModularSchemeValue
                                  Sign
                                  Sign
                                  (CPChar' Sign)
                                  (CP Bool)
                                  ptr 
                                  ptr 
                                  ptr
                                  var
                                  exp
                                  (Map String var)

type instance VarDom (SignValue ptr var exp) = SignValue ptr var exp
type instance PaiDom (SignValue ptr var exp) = SimplePair (SignValue ptr var exp)
type instance VecDom (SignValue ptr var exp) = PIVector (SignValue ptr var exp) (SignValue ptr var exp)
type instance StrDom (SignValue ptr var exp) = SchemeString (CPString' Sign) (SignValue ptr var exp)
