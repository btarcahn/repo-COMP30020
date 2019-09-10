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
    ranklist = nub (map rank sortedCardList)
    
    -- Helper functions
    combinations :: Int -> [a] -> [[a]]
    combinations 0 _  = [[]]
    combinations n xs = [ y:ys | y:xs' <- tails xs
                               , ys <- combinations (n-1) xs']
    {--
    Returns the middle element of the list. The middle element has the
    index equals to length of list `div` 2.
    Returns an error if the list is empty.
    Returns the exact element if the list only has one element.
    -}
    middle :: [a] -> a
    middle [] = error "List is empty!"
    middle [singleton] = singleton
    middle list = list !! (div (length list) 2)
    
    pickLowerRank :: Rank -> [[Card]] -> [Card]
    pickLowerRank _ [] = error "Nothing to choose from!"
    
    
    {--
    Takes in a list of Cards, then returns the Rank that is lowest
    in that list of Cards.
    -}
    lowestRank :: [Card] -> Rank
    lowestRank _cardlist = minimum (map rank _cardlist)
    
    {--
    Takes in a list of Cards, then returns the Rank that is the greatest
    in that list of Cards.
    -}
    highestRank :: [Card] -> Rank
    highestRank _cardlist = maximum (map rank _cardlist)

    {--
    Delete all combinations of Cards that have the specified combination
    of Ranks.
    -}
    destroyRank :: [Rank] -> [[Card]] -> [[Card]]
    destroyRank _ [] = []
    destroyRank _ranklist (d:ds)
        | sort (map rank d) == sort _ranklist = destroyRank _ranklist ds
        | otherwise =  [d] ++ destroyRank _ranklist ds

    onlyRank :: [Rank] -> [[Card]] -> [[Card]]
    onlyRank _ [] = []
    onlyRank _ranklist (d:ds)
        | sort (map rank d) == sort _ranklist = [d] ++ onlyRank _ranklist ds
        | otherwise = onlyRank _ranklist ds

    onlySuit :: [Suit] -> [[Card]] -> [[Card]]
    onlySuit _ [] = []
    onlySuit _suitlist (d:ds)
        | sort (map suit d) == sort _suitlist = [d] ++ onlySuit _suitlist ds
        | otherwise = onlySuit _suitlist ds

    destroySuit :: [Suit] -> [[Card]] -> [[Card]]
    destroySuit _ [] = []
    destroySuit _suitlist (d:ds)
        | sort (map suit d) == sort _suitlist = destroySuit _suitlist ds
        | otherwise =  [d] ++ destroySuit _suitlist ds

    destroyLowerRank :: Rank -> [[Card]] -> [[Card]]
    destroyLowerRank _ [] = []
    destroyLowerRank _rank (p:ps)
        | any (_rank > ) (map rank p) = destroyLowerRank _rank ps
        | otherwise = [p] ++ destroyLowerRank _rank ps

    destroyHigherRank :: Rank -> [[Card]] -> [[Card]]
    destroyHigherRank _ [] = []
    destroyHigherRank _rank (p:ps)
        | any (_rank < ) (map rank p) = destroyHigherRank _rank ps
        | otherwise = [p] ++ destroyHigherRank _rank ps
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
                  = length (intersect (nub (map rank _g)) (nub (map rank _ans)))
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
        -- base case
        | length _prev_guess == a = (_prev_guess, Win True)
        -- some enhancements with extreme cases
        | e == length _prev_guess = (middle _onlied_suits, Remnants _onlied_suits)
        | c == length _prev_guess = (middle _onlied_ranks, Remnants _onlied_ranks)
        | c == 0 = (middle _destroyed_ranks, Remnants _destroyed_ranks)
        | e == 0 = (middle _destroyed_suits, Remnants _destroyed_suits)
        | b == 0 = (middle _destroyed_lower_ranks, Remnants _destroyed_lower_ranks)
        | d == 0 = (middle _destroyed_higher_ranks, Remnants _destroyed_higher_ranks)
        -- continuous determinism
        | otherwise = (middle _next_remnants, Remnants _next_remnants)
        where _next_remnants = delete _prev_guess (filter (\x -> feedback x _prev_guess == (a,b,c,d,e)) _remnants)
              _destroyed_ranks = destroyRank (map rank _prev_guess) _next_remnants
              _destroyed_suits = destroySuit (map suit _prev_guess) _next_remnants
              _destroyed_lower_ranks = delete _prev_guess (destroyLowerRank (lowestRank _prev_guess) _next_remnants)
              _destroyed_higher_ranks = delete _prev_guess (destroyHigherRank (highestRank _prev_guess) _next_remnants)
              _onlied_ranks = delete _prev_guess (onlyRank (map rank _prev_guess) _next_remnants)
              _onlied_suits = delete _prev_guess (onlySuit (map suit _prev_guess) _next_remnants)
    
