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


-- Helper constants
kh = Card Heart King
qh = Card Heart Queen
ks = Card Spade King
qs = Card Spade Queen

-- Helper functions

lowestRank :: [Card] -> Rank
lowestRank _cardlist = minimum (map rank _cardlist)

highestRank :: [Card] -> Rank
highestRank _cardlist = maximum (map rank _cardlist)

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
    where correct_card = length (intersect _g _ans)
          lower_rank = length (filter (< (lowestRank _g)) (map rank _ans))
          correct_rank = length (intersect (nub(map rank _g)) (nub (map rank _ans)))
          higher_rank = length (filter (> (highestRank _g)) (map rank _ans))
          correct_suit = length (intersect (nub (map suit _g)) (nub (map suit _ans)))

initialGuess :: Int -> ([Card], GameState)
initialGuess _ = ([], GameState[])

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess _ _ = ([], GameState[])