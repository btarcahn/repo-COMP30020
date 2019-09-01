sumInts :: [Int] -> Int
sumInts [] = 0
sumInts (x:xs) = x + sumInts xs