{-- 
Project 1's module. This file is equivalent 
to the Proj1.hs file specified on the Grok platform.
Author: Bach Tran (b.tran17@student.unimelb.edu.au)
Student ID: 941113
-}

module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card

data GameState = Won | Lost

feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback _ _ = (0,0,0,0,0)

initialGuess :: Int -> ([Card], GameState)
initialGuess _ = ([], Won)

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess _ _ = ([], Won)