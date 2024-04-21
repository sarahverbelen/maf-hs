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

numPrimitives :: [(String, Int)]
-- a list of primitives that return numbers + the amount of parameters they require
numPrimitives = [("+", 2), ("-", 2), ("*", 2), ("/", 2)]

boolPrimitives :: [(String, Int)]
-- a list of primitives that return booleans + the amount of parameters they require  
boolPrimitives = [("<", 2), ("=", 2)]

genPrimitive :: [(String, Int)] -> Gen (Exp, Int)  
genPrimitive primList = do
  (primName, args) <- oneof $ map return primList
  return (Var (Ide primName NoSpan), args)

genBoolExp :: Gen Exp 
-- | generates expressions that return booleans
genBoolExp = do 
  oneof [
    do b <- arbitrary; return $ Bln b NoSpan,
    do (prim, args) <- genPrimitive boolPrimitives; ops <- vectorOf args (sized genValExp); return $ App prim ops NoSpan
    ]

-- todo: use this somewhere
genStatementExp :: Gen Exp 
-- | generates expressions that don't necessarily have return values (aka cant be used as a right hand side of an assignment)
genStatementExp = oneof [ 
  -- define
  do ide <- arbitrary; e <- genValExp; return $ Dfv ide e NoSpan,
  -- set 
  do ide <- arbitrary; e <- genValExp; return $ Set ide e NoSpan
  ]

-- todo: scope of variables!
genValExp :: Int -> Gen Exp 
-- | generates expressions that return a numeric value 
genValExp 0 = oneof [
      -- simple expressions
      do randNr <- choose (-30, 30); return $ Num randNr NoSpan
      -- , do randNr <- choose (-30, 30); return $ Rea randNr NoSpan
      , do ide <- arbitrary; return $ Var ide
      ]   
genValExp n = oneof [
      -- simple expressions
      do randNr <- choose (-30, 30); return $ Num randNr NoSpan
      --, do randNr <- choose (-30, 30); return $ Rea randNr NoSpan
      , do ide <- arbitrary; return $ Var ide
      -- begin
      , do es <- scale (\n -> quot n 2) $ listOf1 (genValExp (quot n 2)); return $ Bgn es NoSpan
      -- if 
      , do b <- genBoolExp; c <- genValExp (quot n 2) ; a <- genValExp (quot n 2); return $ Iff b c a NoSpan
      -- let
      , genLetExp (quot n 2)
      -- function application 
      , do (prim, args) <- genPrimitive numPrimitives; ops <- vectorOf args (genValExp (quot n 2)); return $ App prim ops NoSpan
      ]       

genLetExp :: Int -> Gen Exp 
-- | generates lets
genLetExp 0 = genValExp 0
genLetExp n = do 
  ides <- scale (\n -> quot n 2) $ listOf arbitrary; 
  e <- scale (\n -> quot n 2) $ listOf (genValExp (quot n 2)); 
  let binds = zip ides e
  body <- genValExp (quot n 2); 
  let' <- oneof $ map return [Let, Ltt, Ltr, Lrr]
  return $ let' binds body NoSpan 

instance Arbitrary Ide where 
  arbitrary = do
    name <- genVarName
    return $ Ide name NoSpan

instance Arbitrary Exp where 
  arbitrary =
    resize 10 $ sized genLetExp

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