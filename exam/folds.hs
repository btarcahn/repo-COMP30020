-- File: folds.hs
-- Author: Bach Tran
-- Revision & playground for fold functions.
-- This material contains elements from the
-- 29 Aug 2019 Lecture, COMP30020 Declarative
-- Programming by Peter Schatle.
-- 2019, University of Melbourne. All rights reserved.
-- 2019, Bach Tran. All rights reserved.

import Prelude hiding (foldl, foldr)

{-- |Note: the type similar to the accumulator
and the Final value is on the LEFT. This explains
why the binary function is (v -> e -> v).
-}
foldl :: (v -> e -> v) -> v -> [e] -> v
foldl _ base [] = base
foldl f base (x:xs) =
    let newbase = f base x in
        foldl f newbase xs

foldr :: (e -> v -> v) -> v -> [e] -> v
foldr _ base [] = base
foldr f base (x:xs) =
    let fxs = foldr f base xs in
        f x fxs

-- e and v of the same type
suml :: Num a => [a] -> a
suml = foldl (+) 0

productl :: Num a => [a] -> a
productl = foldl (*) 1

concatl :: [[a]] -> [a]
concatl = foldl (++) []

sumlengthl :: [[a]] -> Int
sumlengthl = foldl (flip ((+) . length)) 0

sumlengthr :: [[a]] -> Int
sumlengthr = foldr ((+) . length) 0

sum1 :: Num n => [n] -> n
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs

sum2 :: Num n => [n] -> n
sum2 numlist = foldr (+) 0 numlist

sum3 :: Num n => [n] -> n
sum3 numlist = foldl (+) 0 numlist