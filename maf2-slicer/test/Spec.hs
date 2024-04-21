import Syntax.Scheme
import Analysis.Scheme.Primitives

import Slicer
import Dependency.State
import Property.Agreement

import Test.QuickCheck
import qualified Data.Map as Map
import Data.Maybe

-- generators

genVarName :: Gen String 
genVarName = oneof $ map return ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

genPrimitive :: Gen Exp  
genPrimitive = do
  primName <- oneof $ map return $ Map.keys primitivesByName
  return $ Var (Ide primName NoSpan)

genBoolExp :: Gen Exp 
-- | generates expressions that return booleans
genBoolExp = do 
  oneof [
    do ide <- arbitrary; return $ Var ide, 
    do b <- arbitrary; return $ Bln b NoSpan
    -- TODO: function applications that return bools
    ]

genStatementExp :: Gen Exp 
-- | generates expressions that don't necessarily have return values (aka cant be used as a right hand side of an assignment)
genStatementExp = oneof [ 
  -- define
  do ide <- arbitrary; e <- genValExp; return $ Dfv ide e NoSpan,
  -- set 
  do ide <- arbitrary; e <- genValExp; return $ Set ide e NoSpan,
  -- begin
  do exp <- oneof [genValExp, genStatementExp]; return $ Bgn [exp] NoSpan -- TODO: longer lists of exps (but not too long) 
  ]


genValExp :: Gen Exp 
-- | generates expressions that return a value 
genValExp = oneof [
      -- simple expressions
      do randNr <- choose (-30, 30); return $ Num randNr NoSpan
      , do randNr <- choose (-30, 30); return $ Rea randNr NoSpan
      , do ide <- arbitrary; return $ Var ide
      -- begin
      , do exp <- genValExp; return $ Bgn [exp] NoSpan -- TODO: longer lists of exps (but not too long)
      -- if 
      , do b <- genBoolExp; c <- genValExp; a <- genValExp; return $ Iff b c a NoSpan
      -- let
      , genLetExp
      -- function application 
      , do prim <- genPrimitive; a <- genValExp; b <- genValExp; return $ App prim [a, b] NoSpan -- TODO: correct amount of arguments + correct types of arguments
      ] 

genLetExp :: Gen Exp 
-- | generates lets
genLetExp = do 
  ide <- arbitrary; 
  e <- genValExp; 
  body <- genValExp; 
  let' <- oneof $ map return [Let, Ltt, Ltr, Lrr]
  return $ let' [(ide, e)] body NoSpan 

instance Arbitrary Ide where 
  arbitrary = do
    name <- genVarName
    return $ Ide name NoSpan

instance Arbitrary Exp where 
  arbitrary = genLetExp
    --oneof [genStatementExp, genValExp, genBoolExp]
    

-- properties

prop_preserved_semantics :: Exp -> Bool 
prop_preserved_semantics p = 
  let e = fromJust $ parseString $ show p
      var = "x"
      criterion = [(var, PAll)] -- TODO 
      e' = slice e criterion
      s = mempty
      (_, s1) = abstractEval' e s
      (_, s2) = abstractEval' e' s
      v1 = Map.lookup var s1 
      v2 = Map.lookup var s2  
  in v1 == v2

main :: IO ()
main = sample (arbitrary :: Gen Exp)
  --quickCheck prop_preserved_semantics