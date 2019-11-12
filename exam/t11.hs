data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving Show

sample = Node (Node Empty 3 Empty) 2 (Node Empty 4 Empty)

map_tree :: (a -> a) -> Tree a -> Tree a
map_tree _ Empty = Empty
map_tree fmap (Node l v r) =
    let newleft = map_tree fmap l
        newright = map_tree fmap r
        newval = fmap v
    in (Node newleft newval newright)