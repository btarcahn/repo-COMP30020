{-- 
Project 1's module. This file is equivalent 
to the Proj1.hs file specified on the Grok platform.
Author: Bach Tran (b.tran17@student.unimelb.edu.au)
Student ID: 941113

The code below assumes that the set of cards used is the
standard deck excluding two Jokers. Therefore the deck
has 52 cards. Using other types of deck could trigger
undesired behaviors.

-}

module Proj1 (feedback, initialGuess, nextGuess, GameState) where
    import Card
    import Data.List
    import Data.Maybe
    
    {--
    GameState type.
    Contains the information of the Cards between states.
    @author: Bach Tran
    @since 1.0
    -}
    data GameState = Win Bool | Remnants [[Card]]
        deriving Show
    -- Helper constants
    
    totalCards = 52
    r2c = Card Club R2
    as = Card Spade Ace
    sortedCardList = [r2c..as]
    
    -- Helper functions
    combinations :: Int -> [a] -> [[a]]
    combinations 0 _  = [[]]
    combinations n xs = [ y:ys | y:xs' <- tails xs
                               , ys <- combinations (n-1) xs']

    lowestRank :: [Card] -> Rank
    lowestRank _cardlist = minimum (map rank _cardlist)
    
    highestRank :: [Card] -> Rank
    highestRank _cardlist = maximum (map rank _cardlist)
    
    {--
    Warning: this function will throw an error if the
    Card supplied from the start doesn't exist on the cardlist.
    -}
    elevate :: Card -> Int -> Card
    elevate _card _elevation 
        = sortedCardList 
            !! (succIndex - (if succIndex >= totalCards then totalCards else 0))
        where succIndex = fromJust (elemIndex _card sortedCardList) + _elevation
    
    -- TODO implement a more randomized function here!!!
    pickCards :: Int -> [Card] -> [Card]
    pickCards _ [] = []
    pickCards _cardnum _cardlist
        | (_cardnum <= 0) || (_cardnum > 52) = []
        {--
        | otherwise = [(_cardlist !! middle)] 
            ++ (pickCards (_cardnum - 2) (take (middle - 1) _cardlist))
            ++ (pickCards (_cardnum - 2) (drop middle _cardlist))
        where middle = div (length _cardlist) 2 - (if even (length _cardlist) then 1 else 0)
        -}
        | otherwise = take _cardnum _cardlist
    
    destroyRank :: [Rank] -> [[Card]] -> [[Card]]
    destroyRank _ [] = []
    destroyRank _ranklist (d:ds)
        | sort (map rank d) == sort _ranklist = destroyRank _ranklist ds
        | otherwise =  [d] ++ destroyRank _ranklist ds

    destroySuit :: [Suit] -> [[Card]] -> [[Card]]
    destroySuit _suitlist (d:ds)
        | sort (map suit d) == sort _suitlist = destroySuit _suitlist ds
        | otherwise =  [d] ++ destroySuit _suitlist ds
    -- End of helper functions
    
    {--
    Takes two list of Cards: Answer list and Guess list,
    returns a 5-integer tuple.
    @author: Bach Tran
    @since 1.0
    -}    
    feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
    feedback [] [] = (0,0,0,0,0)
    feedback _ans _g 
        = (correct_card, lower_rank, correct_rank, higher_rank, correct_suit)
        where correct_card 
                  = length (intersect _g _ans)
              lower_rank 
                  = length (filter (< (lowestRank _g)) (map rank _ans))
              correct_rank 
                  = length (intersect (nub(map rank _g)) (nub (map rank _ans)))
              higher_rank 
                  = length (filter (> (highestRank _g)) (map rank _ans))
              correct_suit 
                  = length (intersect (nub (map suit _g)) (nub (map suit _ans)))
    
    {--
    Takes an integer, and return the number of card designed for the
    first play of the guesser.
    If the integer supplied is not positive, or is bigger than 52,
    which is the maximum number of cards allowed, this function
    returns an empty list of Card.
    -}
    initialGuess :: Int -> ([Card], GameState)
    initialGuess _cardnum
        | (_cardnum <= 0) || (_cardnum > 52) = ([], Remnants [])
        | otherwise = (middle, Remnants possibilities)
        where possibilities = combinations _cardnum sortedCardList
              middle = possibilities !! ((length possibilities) `div` 2)
    
    {--
    Deduces the next possible guess based on the feedback
    component given by the previous guess.
    -}
    nextGuess :: ([Card], GameState) -> (Int,Int,Int,Int,Int) -> ([Card], GameState)
    nextGuess (_prev_guess, Remnants _remnants) (a, b, c, d, e)
        | length _prev_guess == a = (_prev_guess, Remnants[])
        | c == 0 = (destroyRank (map rank _prev_guess) _remnants !! 0, Remnants (destroyRank (map rank _prev_guess) _remnants))
        | e == 0 = (destroySuit (map suit _prev_guess) _remnants !! 0, Remnants (destroySuit (map suit _prev_guess) _remnants))
        | otherwise = (_next_guess, Remnants _next_remnants)
        where _next_remnants = delete _prev_guess _remnants
              _next_guess = (_next_remnants !! 0)
    