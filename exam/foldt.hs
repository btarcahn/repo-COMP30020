-- File: foldt.hs
-- A place for folding trees

data Btree a = Nil2 | Node2 a (Btree a) (Btree a)
data Ttree a = Nil3 | Node3 a (Ttree a) (Ttree a) (Ttree a)
data Gtree a = NilG | Nodeg [Gtree a]

foldl_btree :: (v -> e -> v) -> v -> Btree e -> v
foldl_btree _ base Nil2 = base
foldl_btree f base (Node 2 x l r) =

foldr_btree :: (e -> v -> v) -> v -> Btree e -> v
foldr_btree _ base Nil2 = base
foldr_btree f base (Node2 x l r) = f x base2
    where base2 = foldr_btree f base3 l
          base3 = foldr_btree f base r

foldr_ttree :: (e -> v -> v) -> v -> Ttree e -> v
foldr_ttree _ base Nil3 = base
foldr_tree f base (Node3 x l m r) = f x base2
    where base2 = foldr_ttree f base3 l
          base3 = foldr_ttree f base4 m
          base4 = foldr_ttree f base r