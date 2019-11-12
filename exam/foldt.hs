-- File: foldt.hs
-- A place for folding trees

import Data.List

data Btree a = Nil2 | Node2 a (Btree a) (Btree a)
data Ttree a = Nil3 | Node3 a (Ttree a) (Ttree a) (Ttree a)
data Gtree a = NilG | Nodeg [Gtree a]

----------------- TEST DATA 2-----------------

bleafOne = Node2 1 Nil2 Nil2
bleafTwo = Node2 2 Nil2 Nil2
bleafThree = Node2 3 Nil2 Nil2
bleafFour = Node2 4 Nil2 Nil2
bleafFive = Node2 5 Nil2 Nil2
bleafSix = Node2 6 Nil2 Nil2
bleafSeven = Node2 7 Nil2 Nil2

btreeOne = Node2 1 bleafTwo bleafThree
btreeTwo = Node2 1 (Node2 2 bleafFour bleafFive) (Node2 3 bleafSix bleafSeven)


--------------- TEST DATA 3 -----------------
leafOne = Node3 1 Nil3 Nil3 Nil3
leafTwo = Node3 2 Nil3 Nil3 Nil3
leafThree = Node3 3 Nil3 Nil3 Nil3
leafFour = Node3 4 Nil3 Nil3 Nil3
leafFive = Node3 5 Nil3 Nil3 Nil3
leafSix = Node3 6 Nil3 Nil3 Nil3
leafSeven = Node3 7 Nil3 Nil3 Nil3
leafEight = Node3 8 Nil3 Nil3 Nil3
leafNine = Node3 9 Nil3 Nil3 Nil3
-----------------------------------------

treeOne = Node3 1 leafTwo (Node3 3 leafSix Nil3 leafNine) leafFour
treeTwo = Node3 3 leafFour (Node3 6 leafFive Nil3 leafSeven) leafSeven
treeThree = Node3 5 leafThree leafSeven leafSix

treeArr = Node3 [1,1,1,1,1,1,1,111,99,111] (Node3 [5,3,7,7,7] Nil3 Nil3 Nil3) (Nil3) (Node3 [1,2,3,4] Nil3 Nil3 Nil3)
------------------- FUNCTIONS ---------------
foldl_btree :: (v -> e -> v) -> v -> Btree e -> v
foldl_btree _ base Nil2 = base
foldl_btree f base (Node2 x l r) = f base2 x
    where base2 = foldl_btree f base3 r
          base3 = foldl_btree f base l

foldr_btree :: (e -> v -> v) -> v -> Btree e -> v
foldr_btree _ base Nil2 = base
foldr_btree f base (Node2 x l r) = f x base2
    where base2 = foldr_btree f base3 l
          base3 = foldr_btree f base r

foldl_ttree :: (v -> e -> v) -> v -> Ttree e -> v
foldl_ttree _ base Nil3 = base
foldl_ttree f base (Node3 x l m r) = f base2 x
    where base2 = foldl_ttree f base3 r
          base3 = foldl_ttree f base4 m
          base4 = foldl_ttree f base l

foldr_ttree :: (e -> v -> v) -> v -> Ttree e -> v
foldr_ttree _ base Nil3 = base
foldr_tree f base (Node3 x l m r) = f x base2
    where base2 = foldr_ttree f base3 l
          base3 = foldr_ttree f base4 m
          base4 = foldr_ttree f base r

btree2listl :: Btree a -> [a]
btree2listl Nil2 = []
btree2listl (Node2 x l r) = foldl_btree (flip (:)) [] (Node2 x l r)

btree2listr :: Btree a -> [a]
btree2listr Nil2 = []
btree2listr (Node2 x l r) = foldr_btree (:) [] (Node2 x l r)

ttree2listr :: Ttree a -> [a]
ttree2listr (Node3 x l m r) = foldr_ttree (:) [] (Node3 x l m r)

similarShape :: Btree a -> Btree a -> Bool
similarShape Nil2 Nil2 = True
similarShape _ Nil2 = False
similarShape Nil2 _ = False
similarShape (Node2 x1 l1 r1) (Node2 x2 l2 r2) =
    similarShape l1 r1 && similarShape l2 r2

averageTtree :: Fractional a => Ttree Integer -> a
averageTtree Nil3 = 0.0
averageTtree (Node3 x l m r) = 
    let (ftotal, fcount) = sumAndCount (Node3 x l m r) (0, 0)
    in (fromIntegral ftotal) / (fromIntegral fcount)

sumAndCount :: Ttree Integer -> (Integer, Integer) -> (Integer, Integer)
sumAndCount Nil3 final_ans = final_ans
sumAndCount (Node3 v l m r) (total0, count0) =
    let (total1, count1) = sumAndCount l (total0, count0)
        (total2, count2) = sumAndCount m (total1, count1)
        (total3, count3) = sumAndCount r (total2, count2)
    in (total3 + v, count3 + 1)

findMinAndConcat :: Ttree [a] -> ([a], [a])
findMinAndConcat Nil3 = ([],[])
findMinAndConcat (Node3 v l m r) = minAndConcat (Node3 v l m r) (v, []) 

minAndConcat :: Ttree [a] -> ([a], [a]) -> ([a], [a])
minAndConcat Nil3 base = base
minAndConcat (Node3 v l m r) (shortest0, concat0) =
    let (shortest1, concat1) = minAndConcat l (shortest0, concat0)
        shortest1' = if length shortest0 < length shortest1
                then shortest0 else shortest1
        (shortest2, concat2) = minAndConcat m (shortest1', concat1)
        shortest2' = if length shortest1' < length shortest2
                then shortest1' else shortest2
        (shortest3, concat3) = minAndConcat r (shortest2', concat2)
        shortest3'=  if length shortest2' < length shortest3
                then shortest2' else shortest3
    in (shortest3', concat3 ++ v)