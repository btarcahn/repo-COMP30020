{-- 
Project 1's module. This file is equivalent 
to the Proj1.hs file specified on the Grok platform.
Author: Bach Tran (b.tran17@student.unimelb.edu.au)
Student ID: 941113
-}

module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List

{--
GameState type.
Contains the information of the Cards between states.
@author: Bach Tran
@since 1.0
-}
data GameState = GameState [Card]
    deriving Show

-- Helper functions
kh = Card Heart King
qh = Card Heart Queen
ks = Card Spade King
qs = Card Spade Queen
{--
Count occurences satisfies a criteria
-}
count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count func list = length (filter func list)

serializedElem :: Eq a => [a] -> [a] -> [Bool]
serializedElem _ans [g] = [elem g _ans] 
serializedElem _ans (g:gs) = (elem g _ans) : serializedElem _ans gs

{--
Returns the count for "how many elements in list
b satisfies the condition bounded to list a"
-}
countAcross :: (Eq a, Ord a) => (a -> [a] -> Bool) -> [a] -> [a] -> Int
countAcross _func _ [] = 0
countAcross _func _a (b:bs)
    | _func b _a = 1 + countAcross _func _a bs
    | otherwise = countAcross _func _a bs
-- End of helper functions

{--
Takes two list of Cards: Answer list and Guess list,
returns a 5-integer tuple.
@author: Bach Tran
@since 1.0
-}    
feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback [] [] = (0,0,0,0,0)
feedback _ans _g = (correct_card, lower_rank, correct_rank, higher_rank, correct_suit)
    where correct_card = countAcross elem (nub _ans) (nub _g)
          lower_rank = 0
          correct_rank = 0
          higher_rank = 0
          correct_suit = 0

initialGuess :: Int -> ([Card], GameState)
initialGuess _ = ([], GameState[])

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess _ _ = ([], GameState[])