-- Lazy evaluation: activated
takeFib :: Int -> [Int]
takeFib n = take n fibInf
    where fibInf = 1:1:(zipWith (+) fibInf $ tail fibInf)
-- Recursion
fib_nth :: Int -> Int
fib_nth 0 = 1
fib_nth 1 = 1
fib_nth i = fib_nth (i-1) + fib_nth (i-2)

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

plus :: Num a => (a, a) -> (a, a)-> (a, a)
plus (a, b) (c, d) = (a + b, c + d)

average :: Num t => Ttree t -> (t, t)
average Nil = (0, 0)
average (Node3 t l m r) =
    (t, 1) `plus` (average l) `plus` (average m) `plus` (average r) 

---------- MONADS ----------

-- Declaration
data MaybeOK t = OK t | Error String
    deriving Show
return x = OK x
(OK x) >>= f = f x
(Error m) >>= _ = Error m

maybe_head :: [a] -> MaybeOK a
maybe_head [] = Error "nothing"
maybe_head list = OK $ head list
maybe_sqrt :: Floating f => f -> MaybeOK f
maybe_sqrt n = OK $ sqrt n
--maybe_sqrt_of_head :: Floating f => [f] -> MaybeOK f
--maybe_head l >>= maybe_sqrt

