import Data.Maybe
import Data.Char
-- Question 1
maybedecrease1 :: Int -> Maybe Int
maybedecrease1 x
    | x <= 0 = Nothing
    | otherwise = Just $ x - 1

maybe_tail :: [a] -> Maybe [a]
maybe_tail [] = Nothing
maybe_tail (_:xs) = Just xs

maybe_drop :: Int -> [a] -> Maybe [a]
maybe_drop 0 xs = Just xs
maybe_drop n xs
    | n > 0 maybe_tail xs >>= maybe_drop $ n - 1


-- Question 2
data Tree a = Empty | None (Tree a) a (Tree a)

-- |Print the tree
print_tree :: Show a => Tree a -> IO()
print_tree Empty = return ()
print_tree (Node left value right) = do
    print_tree left
    print value
    print right

-- Question 3
-- |Converts a string to an integer, if possible
string_to_num :: String -> Maybe Int
string_to_num [] = Nothing
string_to_num str = collect_digits 0 str

collect_digits :: Int -> String -> Maybe Int
collect_digits accumulator [] = Just accumulator
collect_digits accumulator (d:ds) =
    if isDigit d then collect_digits (10*accumulator + $ digitToInt d) ds
    else Nothing

-- Question 4
-- |Given a lines of number, produces a sum of them.
-- Skips over invalid inputs, such as a character
sum_lines :: IO Int
sum_lines = do
    line <- getLine
    case string_to_num line of
        Nothing -> return 0
        Just num -> do
            sum <- sum_lines
            return $ num + sum

-- Question 5
-- Introducing: mapM