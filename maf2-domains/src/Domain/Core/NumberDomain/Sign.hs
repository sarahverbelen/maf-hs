{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Core.NumberDomain.Sign where

import Lattice 
import Domain.Core.NumberDomain.Class 

import Control.Monad.DomainError
import Control.Monad.Join 
import Control.Monad.AbstractM

instance NumberDomain Sign where
    type Boo Sign = CP Bool
    isZero v = return $ Constant ((==) Zero v)
    random _ = return STop

    plus SBottom _ = return SBottom
    plus _ SBottom = return SBottom
    plus Zero v = return v 
    plus v Zero = return v 
    plus ZeroOrNeg Neg = return Neg 
    plus Neg ZeroOrNeg = return Neg
    plus ZeroOrPos Pos = return Pos 
    plus Pos ZeroOrPos = return Pos
    plus Neg Neg = return Neg 
    plus Pos Pos = return Pos 
    plus _ _ = return STop

    minus SBottom _ = return SBottom 
    minus _ SBottom = return SBottom
    minus Zero Neg = return Pos 
    minus Zero Pos = return Neg 
    minus Zero ZeroOrNeg = return ZeroOrPos 
    minus Zero ZeroOrPos = return ZeroOrNeg  
    minus v Zero = return v 
    minus Pos Neg = return Pos 
    minus Pos ZeroOrNeg = return Pos 
    minus Neg Pos = return Neg
    minus Neg ZeroOrPos = return Neg  
    minus _ _ = return STop

    times Zero _ = return Zero 
    times _ Zero = return Zero
    times Pos v = return v 
    times v Pos = return v 
    times Neg Neg = return Pos 
    times Neg ZeroOrNeg = return ZeroOrPos
    times ZeroOrNeg Neg = return ZeroOrPos
    times Neg ZeroOrPos = return ZeroOrNeg
    times ZeroOrPos Neg = return ZeroOrNeg
    times ZeroOrNeg ZeroOrNeg = return ZeroOrPos
    times ZeroOrNeg ZeroOrPos = return ZeroOrNeg 
    times ZeroOrPos ZeroOrNeg = return ZeroOrNeg
    times ZeroOrPos ZeroOrPos = return ZeroOrPos
    times _ _ = return STop

    div = times

    expt SBottom _ = return SBottom
    expt _ SBottom = return SBottom
    expt Zero _ = return Zero
    expt Pos _ = return Pos 
    expt ZeroOrPos _ = return ZeroOrPos
    expt _ _ = return STop

    lt Zero Pos = return $ Constant True 
    lt Zero Neg = return $ Constant False 
    lt ZeroOrNeg Pos = return $ Constant True
    lt ZeroOrPos Neg = return $ Constant False 
    lt Pos Neg = return $ Constant False 
    lt Pos Zero = return $ Constant False 
    lt Neg Pos = return $ Constant True
    lt Neg Zero = return $ Constant True
    lt _ _ = return $ Top

    eq Zero Zero = return $ Constant True 
    eq _ _ = return Top

instance IntDomain Sign where
    type Str Sign = String
    type Rea Sign = Sign
    toReal = return
    toString v = return  $ (show v)
   
    quotient Zero _ = return Zero 
    quotient _ Zero = return Zero
    quotient Pos v = return v 
    quotient v Pos = return v 
    quotient Neg Neg = return Pos 
    quotient Neg ZeroOrNeg = return ZeroOrPos
    quotient ZeroOrNeg Neg = return ZeroOrPos
    quotient Neg ZeroOrPos = return ZeroOrNeg
    quotient ZeroOrPos Neg = return ZeroOrNeg
    quotient ZeroOrNeg ZeroOrNeg = return ZeroOrPos
    quotient ZeroOrNeg ZeroOrPos = return ZeroOrNeg 
    quotient ZeroOrPos ZeroOrNeg = return ZeroOrNeg
    quotient ZeroOrPos ZeroOrPos = return ZeroOrPos
    quotient _ _ = return STop

    remainder _ _ =  return ZeroOrPos
    modulo _ _ = return ZeroOrPos


instance RealDomain Sign where
   type IntR Sign = Sign
   toInt = return
   ceiling = return
   floor = return
   round = return
   log a = --return ZeroOrPos
      domain (Constant $ (== Zero) a) InvalidArgument <||> return ZeroOrPos
   sin Zero = return Zero 
   sin _ = return STop
   asin a = return a
   cos Zero = return Pos 
   cos _ = return STop
   acos _ = return ZeroOrPos
   tan Zero = return Zero 
   tan _ = return STop
   atan a = return a
   sqrt a =
      domain (Constant $ (== Neg) a) InvalidArgument <||> -- todo: what about ZeroOrNeg?
      return ZeroOrPos