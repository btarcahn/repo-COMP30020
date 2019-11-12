import Data.Char

data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving Show

testTree = Node (Node Empty 64 Empty) 32 (Node Empty 128 Empty)

maybe_tail :: [a] -> Maybe [a]
maybe_tail [] = Nothing
maybe_tail (_:xs) = Just xs

maybe_drop :: Int -> [a] -> Maybe [a]
maybe_drop 0 xs = Just xs
maybe_drop n xs
    | n > 0 = maybe_tail xs >>= maybe_drop (n-1)

maybe_drop' :: Int -> [a] -> Maybe [a]
maybe_drop' 0 xs = Just xs
maybe_drop' n xs
    | n > 0 =
        let mt = maybe_tail xs in
        case mt of
            Nothing -> Nothing
            Just xs1 -> maybe_drop' (n-1) xs1

print_tree :: Show a => Tree a -> IO ()
print_tree Empty = return ()
print_tree (Node l v r) = do
    print_tree l
    print v
    print_tree r

str_to_num :: String -> Maybe Int
str_to_num [] = Nothing
str_to_num (d:ds) = str_to_num' 0 (d:ds)

str_to_num' :: Int -> String -> Maybe Int
str_to_num' base [] = Just base
str_to_num' base (d:ds) =
    if isDigit d then 
        str_to_num' (10 * base + digitToInt d) ds
    else Nothing

sum_lines' :: IO Int
sum_lines' = do
    line <- getLine
    case str_to_num line of
        Nothing -> return 0
        Just num -> do
            sum <- sum_lines'
            return (num + sum)

sum_lines :: IO Int
sum_lines = 
    getLine >>=
        \line -> case str_to_num line of
            Nothing -> return 0
            Just num ->
                sum_lines >>=
                    \sum -> return (num+sum)

list_num_lines' :: IO [Int]
list_num_lines' = do
    line <- getLine
    case str_to_num line of
        Nothing -> return []
        Just num -> do
            nums <- list_num_lines'
            return (num:nums)

list_num_lines :: IO [Int]
list_num_lines =
    getLine >>=
        \line -> case str_to_num line of
            Nothing -> return []
            Just num ->
                list_num_lines >>=
                    \nums -> return (num:nums)