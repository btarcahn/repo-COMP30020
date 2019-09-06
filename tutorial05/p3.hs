-- Q1
maybeApply :: (a -> b) -> Maybe a -> Maybe b
maybeApply _ Nothing = Nothing
maybeApply f (Just x) = Just (f x)

-- Question 2
zWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zWith _ [] _ = []
zWith _ _ [] = []
zWith f (x:xs) (y:ys) = (f x y) : (zWith f xs ys)

-- Q3
linearEqn :: Num a => a -> a -> [a] -> [a]
linearEqn _ _ [] = []
linearEqn a b list = map (\x -> a * x + b) list
--linearEqn a b list = map (b+) (map (a *) list)
--linearEqn a b list = ((map (b+)) . (map (a*))) list

-- Q4
sqrtPM :: (Floating a, Ord a) => a -> [a]
sqrtPM x
    | x > 0 = let y = sqrt x in [y, -y]
    | x == 0 = [0]
    | otherwise = []

allsqrts :: (Floating a, Ord a) => [a] -> [a]
allsqrts [] = []
allsqrts list  = foldl (++) [] (map sqrtPM list)

-- concat way
-- allsqrts list = concat (map sqrtPM list)
-- allsqrts list = (concat . (map sqrtPM))
-- allsqrts concatMap sqrtPM list