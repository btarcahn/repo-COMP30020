-- File: folds.hs
-- Author: Bach Tran
-- Revision & playground for fold functions.
-- This material contains elements from the
-- 29 Aug 2019 Lecture, COMP30020 Declarative
-- Programming by Peter Schatle.
-- 2019, University of Melbourne. All rights reserved.
-- 2019, Bach Tran. All rights reserved.

{--
Notes: these fold function primarily works on list.
-}

import Prelude hiding (foldl, foldr, length, map, sum,
    product, reverse)

{-- |Note: the type similar to the accumulator
and the Final value is on the LEFT. This explains
why the binary function is (v -> e -> v).
This function is tail recursive, and lazy
-}
foldl :: (v -> e -> v) -> v -> [e] -> v
foldl _ base [] = base
foldl f base (x:xs) =
    let newbase = f base x in
        foldl f newbase xs
{-- |Note: the type similar to the accumulator
and the Final value is on the RIGHT. This explains
why the binary function is (e -> v -> v).
This function is NOT tail recursive.
-}
foldr :: (e -> v -> v) -> v -> [e] -> v
foldr _ base [] = base
foldr f base (x:xs) =
    let fxs = foldr f base xs in
        f x fxs

foldb :: (e -> e -> e) -> e -> [e] -> e
foldb _ b [] = b
foldb _ _ [x] = x
foldb f b l@(_:_:_) =
    let
        len = length l
        (half1, half2) = splitAt (div len 2) l
        value1 = foldb f b half1
        value2 = foldb f b half2
    in
        f value1 value2

-- e and v of the same type
sum :: Num a => [a] -> a
sum list = foldl (+) 0 list

sum_deep :: Num a => [[a]] -> a
sum_deep list = foldr ((+) . sum) 0 list

product :: Num a => [a] -> a
product list = foldl (*) 1 list

{--
Cost of (++) is asymmetrical, it
depends on the LEFT argument.
e.g. [long_list] ++ [long_list]
You have to RUN THROUGH the first
argument of (++) to calculate the
concatenation.
-}

concatl :: [[a]] -> [a]
concatl = foldl (++) []
{-- |Note: this function performs
better than foldr, since the RIGHT
argument of (++) is much less costly.
-}
concatr :: [[a]] -> [a]
concatr = foldr (++) []

sumlengthl :: [[a]] -> Int
sumlengthl = foldl (flip ((+) . length)) 0

sumlengthr :: [[a]] -> Int
sumlengthr = foldr ((+) . length) 0

length :: [a] -> Int
length list = foldr ((+) . const 1) 0 list

map :: (a -> b) -> [a] -> [b]
map f origin = foldr ((:) . f) [] origin

-- QUIZ
hypotenuse :: [Double] -> Double
hypotenuse sides = sqrt $ foldr1 ((+) . (**2)) sides

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []