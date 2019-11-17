-- File: main.hs
-- Copyright: 2019 Bach Tran. All rights reserved.
-- This is the answer key for the Haskell question,
-- in the Practice exam for COMP30026 Models of
-- Computation, University of Melbourne 2019, Sem 2.

import Data.List
import Data.String

-- |Main function, just for a successful compilation.
main :: IO ()
main = return ()

-- |Given the function in the format of a tuple (a,a),
-- map the first argument to the second argument.
-- This function assumes that the [(a,a)] given is total.
apply :: (Eq a, Num a) => a -> [(a, a)] -> a
apply x (i:is)
  | not $ elem x $ map fst (i:is) = error "Undefined"
  | x == fst i = snd i
  | otherwise = apply x is

-- |Checks if a function given is a total function
-- under the Integer n.
-- This function only works for Integer data type.
isTotalFct :: Integer -> [(Integer, Integer)] -> Bool
isTotalFct n instructions
  | n < 0 = False
  | otherwise = (sort . nub) (map fst instructions) == [0..n]

-- |Checks if a functon given is an isIdempotent under
-- the Integer n. This function only works for Integer
-- data type.
isIdempotent :: Integer -> [(Integer, Integer)] -> Bool
isIdempotent 0 f = 
  let first = apply 0 f 
      second = apply first f
  in elem (0, first) f && elem (0, second) f
isIdempotent n f 
  | n < 0 = False
  | otherwise =
    let first = apply n f 
        second = apply first f
    in elem (0, first) f && elem (0, second) f
      && isIdempotent (n-1) f

-- NOTE: FROM THIS PART ON, THE DEFINITION OF A FUNCTION
-- FOLLOWS THE RULE IN THE 2009 DISCRETE STRUCTURES EXAM.

-- |Returns True if the array [(a,b)] supplied is actually
-- a function. This ensures the distinction between a
-- function, and a set of relations.
isFunction :: (Eq a, Eq b) => [(a,b)] -> Bool
isFunction [] = False
isFunction f = len_domain == len_domainraw
  where len_domain = length $ nub $ map fst f
        len_domainraw = length $ map fst f

-- |Returns True if all the members from the first
-- list exists in the second list.
isSublist :: Eq a => [a] -> [a] -> Bool
isSublist [] _ = True
isSublist (x:xs) b = elem x b && isSublist xs b

-- |Returns True if the function supplied is total.
-- In a total function, every element in its domain
-- has map to its codomain.
isTotal :: (Eq a, Eq b) => [a] -> [(a,b)] -> Bool
isTotal domain f = isFunction f
  && isSublist domain (map fst f)

-- |Returns True if the function is injective.
-- In an injective function, each element in the domain
-- is mapped to distinct elements in the codomain.
-- i.e. f(x) == f(y) => x == y
isInjective :: (Eq a, Eq b) => [(a,b)] -> Bool
isInjective f = isFunction f 
  && len_codomain == len_raw
  where len_codomain = length $ nub $ map snd f
        len_raw = length $ map snd f
-- |Returns True if the function is surjective.
-- In a surjective function, each element in the codomain
-- always has a map from the domain.
-- i.e. forall y in codomain, exists x in domain: f(x) = y. 

isSurjective :: (Eq a, Eq b) => [a] -> [b] -> [(a,b)] -> Bool
isSurjective [] _ _ = False
isSurjective _ [] _ = False
isSurjective dom codom f = isFunction f
  && isSublist (map snd f) codom

-- Returns True if the function is bijective.
-- A bijective function is injective && surjective.
-- Therefore, this function combines isInjective && isSurjective.
isBijective :: (Eq a, Eq b) => [a] -> [b] -> [(a,b)] -> Bool
isBijective dom codom f = isInjective f 
  && isSurjective dom codom f

-- NOTE: THE FOLLOWING SECTION DEALS WITH BOOLEAN EXPRESSION
-- WITH THE TYPE DECLARATION BELOW:
data BoolExp =
  Var String
  | NOT BoolExp
  | AND BoolExp BoolExp
  | OR BoolExp BoolExp
  deriving Show

testExpr = AND (NOT (AND (Var "a") (Var "b"))) (Var "c")

-- QUESTION 1b (2009)
-- |Convert any Boolean expression with AND, OR, NOT
-- to negative normal form.
nnf :: BoolExp -> BoolExp
nnf (Var variable) = Var variable
nnf (NOT (Var variable)) = NOT (Var variable)
nnf (NOT (NOT expr)) = nnf expr
nnf (NOT (AND a b)) = OR (nnf (NOT a)) (nnf (NOT b))
nnf (NOT (OR a b)) = AND (nnf (NOT a)) (nnf (NOT b))
nnf (AND a b) = AND (nnf a) (nnf b)
nnf (OR a b) = OR (nnf a) (nnf b)
