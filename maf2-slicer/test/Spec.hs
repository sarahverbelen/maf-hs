import Syntax.Scheme

import Slicer
import Dependency.State
import Property.Agreement

import Test.QuickCheck
import qualified Data.Map as Map

-- generators

-- properties

prop_preserved_semantics :: Exp -> Bool 
prop_preserved_semantics e = 
  let var = "x"
      criterion = [(var, PAll)] -- TODO 
      e' = slice e criterion
      s = mempty
      (_, s1) = abstractEval' e s
      (_, s2) = abstractEval' e' s
      v1 = Map.lookup var s1 
      v2 = Map.lookup var s2  
  in v1 == v2

main :: IO ()
main = quickCheck prop_preserved_semantics