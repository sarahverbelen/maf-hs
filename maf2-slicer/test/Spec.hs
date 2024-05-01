module Spec where

import Syntax.Scheme
import Analysis.Scheme.Primitives

import Slicer
import Dependency.State
import Property.Agreement

import Test.QuickCheck
import qualified Data.Map as Map
import Data.Maybe
import Data.List ((\\), elem, zipWith)

-- generators

type Context = ([Ide], [Ide]) -- (defined variables, initialized variables)

genLetter :: Gen Char 
genLetter = oneof $ map return ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']

genFreshVarName :: [String] -> Gen String 
genFreshVarName vs = (resize 6 $ listOf1 genLetter) `suchThat` (\nm -> not $ nm `elem` vs)

genFreshIde :: Context -> Gen Ide
genFreshIde (vs, _) = do 
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
genBoolExp ([], []) _ = do b <- arbitrary; return $ Bln b NoSpan
genBoolExp vs n = do 
  oneof [
    do b <- arbitrary; return $ Bln b NoSpan,
    do (prim, args) <- genPrimitive boolPrimitives; ops <- vectorOf args (genSimpleExp vs); return $ App prim ops NoSpan
    ]

genStatementExp :: Context -> Int -> Gen (Exp, Context)
-- | generates expressions that don't necessarily have return values (aka cant be used as a right hand side of an assignment)
genStatementExp vs@(_, []) _ = do e <- genValExp vs 0; return (e, vs)
genStatementExp vs@(defined, initialized)  n = frequency [ 
  -- define
  --(1, do ide <- elements vs; e <- genValExp vs (quot n 2); return (Dfv ide e NoSpan, vs ++ [ide])),
  -- set 
  (2, do ide <- elements initialized; e <- genValExp vs (quot n 2); return (Set ide e NoSpan, vs))
  ]

genSimpleExp :: Context -> Gen Exp 
genSimpleExp (_, []) = do randNr <- choose (-30, 30); return $ Num randNr NoSpan
genSimpleExp vs@(defined, initialized) = frequency [
      (3, do randNr <- choose (-30, 30); return $ Num randNr NoSpan),
      -- (2, do randNr <- choose (-30, 30); return $ Rea randNr NoSpan),
      (2, do ide <- elements initialized; return $ Var ide),
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


genBinds :: Bool -> Context -> Int -> Gen [(Ide, Exp)]
genBinds _ _ 0 = return []
genBinds recursive vs n = do k <- choose (1, n)
                             if recursive 
                                then genRecBinds vs k
                                else genNonRecBinds vs k 

genRecBinds :: Context -> Int -> Gen [(Ide, Exp)]
genRecBinds _ 0 = return []
genRecBinds vs@(defined, initialized) n = do 
  ide <- genFreshIde vs
  e <- genValExp vs (quot n 2)
  nextBds <- genRecBinds (ide:defined, initialized) (quot n 2)
  return $ (ide, e) : nextBds

genNonRecBinds :: Context -> Int -> Gen [(Ide, Exp)]
genNonRecBinds _ 0 = return []
genNonRecBinds vs@(defined, initialized) n = do 
  ide <- genFreshIde vs
  e <- genValExp vs (quot n 2)
  nextBds <- genNonRecBinds (ide:defined, initialized) (quot n 2)
  return $ (ide, e) : nextBds  

genLetExp :: Context -> Int -> Gen Exp 
-- | generates lets
genLetExp vs 0 = genSimpleExp vs
genLetExp vs@(defined, initialized) n = do 
  recursive <- arbitrary
  let possibleLets = if recursive then [Ltt, Lrr] else [Let, Ltt]
  binds <- genBinds recursive vs (quot n 2)
  let ides = map fst binds
  let newVs = (defined ++ ides, initialized ++ ides)
  body <- genValExp newVs (quot n 2); 
  let' <- oneof $ map return possibleLets
  return $ let' binds body NoSpan 

countSets :: Exp -> Int 
countSets (Iff b c a _) = countSets b + countSets c + countSets a
countSets (Bgn es _) = foldr (\ a b -> b + countSets a) 0 es 
countSets (Dfv _ e _) = countSets e 
countSets (Dff _ _ e _) = countSets e 
countSets (Set _ e _) = 1 + countSets e 
countSets (Let bds bdy _) = foldr ((\ a b -> b + countSets a) . snd) 0 bds + countSets bdy
countSets (Ltt bds bdy _) = foldr ((\ a b -> b + countSets a) . snd) 0 bds + countSets bdy 
countSets (Ltr bds bdy _) = foldr ((\ a b -> b + countSets a) . snd) 0 bds + countSets bdy 
countSets (Lrr bds bdy _) = foldr ((\ a b -> b + countSets a) . snd) 0 bds + countSets bdy 
countSets (App op ops _) = countSets op + foldr (\ a b -> b + countSets a) 0 ops
countSets _ = 0

genExpManySets :: Gen Exp 
genExpManySets = (arbitrary :: Gen Exp) `suchThat` (\e -> countSets e > 4)

instance Arbitrary Ide where 
  arbitrary = do
    name <- genFreshVarName []
    return $ Ide name NoSpan  

instance Arbitrary Exp where 
  arbitrary =
    resize 100 $ sized (genLetExp ([], []))

-- properties

testSlice :: Int -> Exp -> Exp 
testSlice i e =   
  let vsInExp = getVarsFromExp' e
      var = vsInExp !! (i `mod` length vsInExp)
      criterion = [(var, PAll)]
  in slice e criterion


prop_preserved_semantics :: Int -> Exp -> Bool 
prop_preserved_semantics i p = 
  let e = fromJust $ parseString $ show p
      vsInExp = getVarsFromExp' e
      var = vsInExp !! (i `mod` length vsInExp)
      e' = testSlice i e
      s = mempty
      (_, s1) = abstractEval' e s
      (_, s2) = abstractEval' e' s
      v1 = Map.lookup var s1 
      v2 = Map.lookup var s2 
  in if (null vsInExp) then True else v1 == v2 