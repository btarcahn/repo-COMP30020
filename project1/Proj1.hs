{-- 
Project 1's module. This file is equivalent 
to the Proj1.hs file specified on the Grok platform.
Author: Bach Tran (b.tran17@student.unimelb.edu.au)
Student ID: 941113
-}

module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card

-- first representation of the state
data GameState = GameState [Card]
    deriving Show

feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)

feedback [] [] = (0,0,0,0,0)
feedback ans g = (correct_card, lower_rank, correct_rank, higher_rank, correct_suit)
    where correct_card = 0
          lower_rank = 0
          correct_rank = 0
          higher_rank = 0
          correct_suit = 0

initialGuess :: Int -> ([Card], GameState)
initialGuess _ = ([], GameState[])

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess _ _ = ([], GameState[])