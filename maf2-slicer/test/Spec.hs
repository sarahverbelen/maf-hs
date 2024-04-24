import Syntax.Scheme
import Analysis.Scheme.Primitives

import Slicer
import Dependency.State
import Property.Agreement

import Test.QuickCheck
import qualified Data.Map as Map
import Data.Maybe
import Data.List ((\\))

-- generators

type Context = [Ide] -- the context contains all variables that are defined in the current environment

varNames :: [String]
varNames = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

genVarName :: Gen String 
genVarName = oneof $ map return varNames 

genFreshVarName :: [String] -> Gen String 
genFreshVarName vs = oneof $ map return (varNames \\ vs)

genFreshIde :: Context -> Gen Ide
genFreshIde vs = do 
  nm <- genFreshVarName (map name vs)
  return $ Ide nm NoSpan

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

genBoolExp :: Context -> Int -> Gen Exp 
-- | generates expressions that return booleans
genBoolExp _ 0  = do b <- arbitrary; return $ Bln b NoSpan
genBoolExp [] _ = do b <- arbitrary; return $ Bln b NoSpan
genBoolExp vs n = do 
  oneof [
    do b <- arbitrary; return $ Bln b NoSpan,
    do (prim, args) <- genPrimitive boolPrimitives; ops <- vectorOf args (genValExp vs (quot n 2)); return $ App prim ops NoSpan
    ]

genStatementExp :: Context -> Int -> Gen (Exp, Context)
-- | generates expressions that don't necessarily have return values (aka cant be used as a right hand side of an assignment)
genStatementExp [] _ = do e <- genValExp [] 0; return (e, [])
genStatementExp vs n = frequency [ 
  -- define
  --(1, do ide <- elements vs; e <- genValExp vs (quot n 2); return (Dfv ide e NoSpan, vs ++ [ide])),
  -- set 
  (2, do ide <- elements vs; e <- genValExp vs (quot n 2); return (Set ide e NoSpan, vs))
  ]

genSimpleExp :: Context -> Gen Exp 
genSimpleExp [] = do randNr <- choose (-30, 30); return $ Num randNr NoSpan
genSimpleExp vs = frequency [
      (3, do randNr <- choose (-30, 30); return $ Num randNr NoSpan),
      -- , do randNr <- choose (-30, 30); return $ Rea randNr NoSpan
      (2, do ide <- elements vs; return $ Var ide),
      (2, do (prim, args) <- genPrimitive numPrimitives; ops <- vectorOf args (genSimpleExp vs); return $ App prim ops NoSpan)
      ] 

genValExp :: Context -> Int -> Gen Exp 
-- | generates expressions that return a numeric value 
genValExp vs 0 = genSimpleExp vs
genValExp vs n = frequency [
      -- simple expressions
      (6, genSimpleExp vs),
      -- begin
      (2, genBeginExp vs (quot n 2)),
      -- if 
      (2, do b <- genBoolExp vs (quot n 2); c <- genValExp vs (quot n 2) ; a <- genValExp vs (quot n 2); return $ Iff b c a NoSpan),
      -- let
      (1, genLetExp vs (quot n 2)),
      -- function application 
      (3, do (prim, args) <- genPrimitive numPrimitives; ops <- vectorOf args (genValExp vs (quot n 2)); return $ App prim ops NoSpan)
      ]       

genBeginExp :: Context -> Int -> Gen Exp 
genBeginExp vs 0 = genSimpleExp vs
genBeginExp vs n = do k <- choose (1, n)
                      exps <- genBeginExp' vs k
                      return $ Bgn exps NoSpan

genBeginExp' :: Context -> Int -> Gen [Exp]
genBeginExp' vs 0 = do e <- genSimpleExp vs; return [e]
genBeginExp' vs n = do 
  (statement, vs') <- genStatementExp vs n
  includeStatement <- frequency [(2, return False), (1, return True)]
  e <- genValExp vs (quot n 2)
  let nextVs = if includeStatement then vs' else vs 
  nextExps <- genBeginExp' nextVs (quot n 2)
  let currExp = if includeStatement then statement else e
  return $ (currExp : nextExps)


genBinds :: Context -> Int -> Gen [(Ide, Exp)]
genBinds _ 0 = return []
genBinds vs n = do k <- choose (1, n)
                   genBinds' vs k

genBinds' :: Context -> Int -> Gen [(Ide, Exp)]
genBinds' _ 0 = return []
genBinds' vs n = do 
  ide <- genFreshIde vs
  e <- genValExp vs (quot n 2)
  nextBds <- genBinds (vs ++ [ide]) (quot n 2)
  return $ (ide, e) : nextBds

genLetExp :: Context -> Int -> Gen Exp 
-- | generates lets
genLetExp vs 0 = genSimpleExp vs
genLetExp vs n = do 
  binds <- genBinds vs (quot n 2)
  let newVs = vs ++ (map fst binds)
  body <- genValExp newVs (quot n 2); 
  let' <- oneof $ map return [Ltt, Lrr] --[Let, Ltt, Ltr, Lrr] -- todo: fix non-recursive let bindings
  return $ let' binds body NoSpan 

instance Arbitrary Ide where 
  arbitrary = do
    name <- genVarName
    return $ Ide name NoSpan

instance Arbitrary Exp where 
  arbitrary =
    resize 10 $ sized (genLetExp [])

-- properties

prop_preserved_semantics :: Exp -> Bool 
prop_preserved_semantics p = 
  let e = fromJust $ parseString $ show p
      vsInExp = getVarsFromExp' e
      var = head vsInExp -- todo: pick random var from expression to test?
      criterion = [(var, PAll)]
      e' = slice e criterion
      s = mempty
      (_, s1) = abstractEval' e s
      (_, s2) = abstractEval' e' s
      v1 = Map.lookup var s1 
      v2 = Map.lookup var s2 
  in if (null vsInExp) then True else v1 == v2 

main :: IO ()
main = do 
  sample (arbitrary :: Gen Exp)
  quickCheck prop_preserved_semantics