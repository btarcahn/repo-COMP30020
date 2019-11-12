-- File: folds.hs
-- Author: Bach Tran
-- Revision & playground for fold functions.
-- 2019, Bach Tran. All rights reserved.

-- import Prelude hiding (foldr, foldl)

veryBigList = [1..1000000]

sum1 :: Num n => [n] -> n
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs

sum2 :: Num n => [n] -> n
sum2 numlist = foldr (+) 0 numlist

sum3 :: Num n => [n] -> n
sum3 numlist = foldl (+) 0 numlist

