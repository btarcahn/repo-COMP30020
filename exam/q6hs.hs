-- This file contains functions that might
-- be similar to Past exam 2018, COMP90048.
data Ttree t = Nil | Node3 t (Ttree t) (Ttree t) (Ttree t)

leafOne = Node3 1 Nil Nil Nil
leafTwo = Node3 2 Nil Nil Nil
leafThree = Node3 3 Nil Nil Nil
leafFour = Node3 4 Nil Nil Nil
leafFive = Node3 5 Nil Nil Nil
leafSix = Node3 6 Nil Nil Nil
leafSeven = Node3 7 Nil Nil Nil
leafEight = Node3 8 Nil Nil Nil
leafNine = Node3 9 Nil Nil Nil
-----------------------------------------

treeOne = Node3 1 leafTwo (Node3 3 leafSix Nil leafNine) leafFour
treeTwo = Node3 3 leafFour (Node3 6 leafFive Nil leafSeven) leafSeven
treeThree = Node3 5 leafThree leafSeven leafSix

sameShape :: (Ttree a) -> (Ttree b) -> Bool
sameShape Nil Nil = True
sameShape Nil _ = False
sameShape _ Nil = False
sameShape (Node3 val1 l1 m1 r1) (Node3 val2 l2 m2 r2) =
    sameShape l1 l2 &&
    sameShape m1 m2 &&
    sameShape r1 r2

foldr_ttree :: (e -> v -> v) -> v -> Ttree e -> v
foldr_ttree _ base Nil = base
foldr_tree f base (Node3 x l m r) = f x base2
    where base2 = foldr_ttree f base3 l
          base3 = foldr_ttree f base4 m
          base4 = foldr_ttree f base r