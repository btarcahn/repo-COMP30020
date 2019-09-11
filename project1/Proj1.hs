{-- 

    | File: Proj1.hs
    | Author: Bach Tran (941113)
    | Email: b.tran17@student.unimelb.edu.au
    | Subject: COMP30020 Declarative Programming
    | Coordinator: Peter Schachte
    | University of Melbourne, 2019.

* I (Bach Tran) do not own the copyright of the code
outside this file.

* Until further notice, any unauthorized distribution 
of this code is not permitted.

$$$$ ASSUMPTION $$$$

* The code below uses the standard set of 52 cards excluding two Jokers. 
Using other types of deck may trigger undesired behaviors.

$$$$ CONTRIBUTING/STYLE $$$$

1. Function names follows camelCaseNamingConvention.
2. Function parameters follows underscore_naming_convention.
3. Definitions under the "where" statement starts with _underscore.
4. List naming, write as one word, e.g. cardlist, suitlist, namelist.
5. One line of code should not be longer than 80 characters.

-}

module Proj1 (feedback, initialGuess, nextGuess, GameState) where
    import Card
    import Data.List
    import Data.Maybe
    
    {-- | A GameState preserves the list of possible combinations
    of Cards that could be the answer of the Guessing game.
    A GameState can also indicates whether the game has reached
    winning stage or not.
    -}
    data GameState = Win Bool | Remnants [[Card]]
        deriving Show
    
    -- Helper functions

    {--| Takes an integer k and a non-empty list, returns a list of
    combinations as a result of "choosing k from the list"
    -}
    combinations :: Int -> [a] -> [[a]]
    combinations _ [] = error "Nothing to choose from!"
    combinations 0 _  = [[]]
    combinations k xs
        | k < 0 = error "k must be non-negative"
        | otherwise =[y:ys | y:xs' <- tails xs, ys <- combinations (k-1) xs']
    {--| Returns the middle element of the list. The middle element has the
    index equals to length of list `div` 2.
    Returns an error if the list is empty.
    Returns the exact element if the list only has one element.
    -}
    middle :: [a] -> a
    middle [] = error "List is empty!"
    middle [singleton] = singleton
    middle list = list !! (div (length list) 2)
    
    {--|
    Takes in a non-empty list of Cards. 
    Returns the Rank that is lowest in that list of Cards.
    -}
    lowestRank :: [Card] -> Rank
    lowestRank [] = error "List is empty!"
    lowestRank cardlist = minimum (map rank cardlist)
    
    {--|
    Takes in a non-empty list of Cards
    Returns the Rank that is the greatest in that list of Cards.
    -}
    highestRank :: [Card] -> Rank
    highestRank [] = error "List is empty!"
    highestRank cardlist = maximum (map rank cardlist)

    {--|
    Takes in a combination of Ranks, and a combination of Cards.
    Deletes all combinations of Cards having that specified combination
    of Ranks.
    This is an inverse function of onlyRank.
    -}
    destroyRank :: [Rank] -> [[Card]] -> [[Card]]
    destroyRank _ [] = []
    destroyRank [] possibilities = possibilities
    destroyRank ranklist (d:ds)
        | sort (map rank d) == sort ranklist = destroyRank ranklist ds
        | otherwise =  d : (destroyRank ranklist ds)
    
    {--|
    Takes in a combination of Ranks, and a combination of Cards.
    Keeps only the combinations of Cards having that specified
    combination of Ranks.
    This is an inverse function of destroyRank.
    -}
    onlyRank :: [Rank] -> [[Card]] -> [[Card]]
    onlyRank _ [] = []
    onlyRank [] possibilities = []
    onlyRank ranklist (d:ds)
        | sort (map rank d) == sort ranklist = d : (onlyRank ranklist ds)
        | otherwise = onlyRank ranklist ds

    {--|
    Takes in a combination of Suits, and a combination of Cards.
    Deletes all combinations of Cards having that specified
    combination of Suits.
    This is an inverse function of onlySuit.
    -}
    destroySuit :: [Suit] -> [[Card]] -> [[Card]]
    destroySuit _ [] = []
    destroySuit [] possibilities = possibilities
    destroySuit _suitlist (d:ds)
        | sort (map suit d) == sort _suitlist = destroySuit _suitlist ds
        | otherwise =  d : (destroySuit _suitlist ds)
    {--|
    Takes in a combination of Suits, and a combination of Cards.
    Keeps only the combinations of Cards having that specified
    combination of Suits.
    This is an inverse function of destroySuit.
    -}
    onlySuit :: [Suit] -> [[Card]] -> [[Card]]
    onlySuit _ [] = []
    onlySuit [] possibilities = possibilities
    onlySuit _suitlist (d:ds)
        | sort (map suit d) == sort _suitlist = d : (onlySuit _suitlist ds)
        | otherwise = onlySuit _suitlist ds

    {--| 
    Takes a list of Card choices (c:cs) and a specifiedRank. 
    Eliminates all choices containing a rank < specifiedRank.
    -}
    destroyLowerRank :: Rank -> [[Card]] -> [[Card]]
    destroyLowerRank _ [] = []
    destroyLowerRank specified_rank (c:cs)
        | any (specified_rank > ) (map rank c) 
            = destroyLowerRank specified_rank cs
        | otherwise = c : (destroyLowerRank specified_rank cs)
    
    {--| 
    Takes a list of Card choices (c:cs) and a specifiedRank, 
    eliminates all choices containing a rank > specifiedRank.
    -}
    destroyHigherRank :: Rank -> [[Card]] -> [[Card]]
    destroyHigherRank _ [] = []
    destroyHigherRank specified_rank (c:cs)
        | any (specified_rank < ) (map rank c) 
            = destroyHigherRank specified_rank cs
        | otherwise = c : (destroyHigherRank specified_rank cs)
    -- End of helper functions
    
    {--|
    Takes two lists of Card choice: Answer and Guess.
    Compute the feedback, which is a 5-Int tuple:
    1. exact_match: 
    how many Cards matches between the Answer and the Guess.
    2. lower_rank: 
    how many cards in the Answer have lower rank than
    the lowest rank in the Guess.
    3. correct_rank: 
    how many matches in terms of Rank.
    4. higher_rank: 
    how many cards in the Answer have higher rank than
    the highest rank in the Guess.
    5. correct_suit: 
    how many matches in terms of Suit.
    -}    
    feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
    feedback [] [] = (0, 0, 0, 0, 0)
    feedback answer guess 
        = (correct_card, lower_rank, correct_rank, higher_rank, correct_suit)
        where correct_card 
                  = length (intersect guess answer)
              lower_rank 
                  = length 
                  (filter (< (lowestRank guess)) (map rank answer))
              correct_rank 
                  = length 
                  (intersect (nub (map rank guess)) (nub (map rank answer)))
              higher_rank 
                  = length 
                  (filter (> (highestRank guess)) (map rank answer))
              correct_suit 
                  = length 
                  (intersect (nub (map suit guess)) (nub (map suit answer)))
    
    {--
    Takes an integer, and return the number of card designed for the
    first play of the guesser.
    If the integer supplied is not positive, or is bigger than 52,
    which is the maximum number of cards allowed, this function
    returns an empty list of Card.
    -}
    initialGuess :: Int -> ([Card], GameState)
    initialGuess cardnum
        | (cardnum <= 0) || (cardnum > 52) = ([], Remnants [])
        | otherwise = (middle, Remnants _possibilities)
        where _possibilities 
                = combinations cardnum [Card Club R2 .. Card Spade Ace]
              middle = _possibilities !! ((length _possibilities) `div` 2)
    
    {--
    Deduces the next possible guess based on the feedback
    component given by the previous guess.
    -}
    nextGuess :: ([Card], GameState) -> (Int,Int,Int,Int,Int) 
        -> ([Card], GameState)
    nextGuess (prev_guess, Remnants remnants) 
      (exact_match, lower_rank, correct_rank, higher_rank, correct_suit)
        -- base case (Win State), when exact match has been reach.
        | length prev_guess == exact_match 
            = (prev_guess, Win True)
        {--
        The following code below is a minor enhancement if we are lucky
        enough to choose a combination of Cards falling into 1 of these
        6 extreme cases. This enhancement may significantly rule out
        more inconsistent possibilities after doing the Primitive filtering.
        -}
        -- 1. If an exact combination of suits has been reached,
        -- eliminates all other irrelevant combinations of ranks.
        | correct_suit == length prev_guess 
            = (middle _onlied_suits, Remnants _onlied_suits)
        -- 2. If an exact combination of ranks has been reached,
        -- eliminates all other irrelevant combinations of ranks.
        | correct_rank == length prev_guess 
            = (middle _onlied_ranks, Remnants _onlied_ranks)
        -- 3. If this combination of ranks is completely wrong,
        -- eliminates all other choices that have this rank combination.
        | correct_rank == 0 
            = (middle _destroyed_ranks, Remnants _destroyed_ranks)
        -- 4. If this combination of suits is completely wrong,
        -- eliminates all other choices that have this suit combination.
        | correct_suit == 0 
            = (middle _destroyed_suits, Remnants _destroyed_suits)
        -- 5. Correct lowest rank! Eliminates all possibilities that
        -- has rank < lowestRank.
        | lower_rank == 0 
            = (middle _destroyed_lower_ranks, Remnants _destroyed_lower_ranks)
        -- 6. Correct highest rank! Eliminates all possibilities that
        -- has rank > highestRank.
        | higher_rank == 0
            = (middle _destroyed_higher_ranks, 
            Remnants _destroyed_higher_ranks)
        -- non-extreme cases: just apply the Primitive filtering
        | otherwise = (middle _next_remnants, Remnants _next_remnants)
        where 
              -- The Primitive filtering:
              -- chooses a possibility only if 
              -- feedback(possibility, previousGuess) 
              -- /= feedback(answer, previousGuess)
              _next_remnants
                = delete prev_guess 
                    (filter (\x -> feedback x prev_guess == 
                        (exact_match, lower_rank, 
                        correct_rank, higher_rank, 
                        correct_suit)) remnants)
              -- removes completely incorrect ranks.
              _destroyed_ranks 
                = destroyRank (map rank prev_guess) _next_remnants
              -- removes completely incorrect suits.
              _destroyed_suits 
                = destroySuit (map suit prev_guess) _next_remnants
              -- removes incorrect lower ranks.
              _destroyed_lower_ranks 
                = delete prev_guess 
                (destroyLowerRank (lowestRank prev_guess) _next_remnants)
              -- removes incorrect higher ranks.
              _destroyed_higher_ranks 
                = delete prev_guess 
                (destroyHigherRank (highestRank prev_guess) _next_remnants)
              -- only chooses the correct combination of ranks.
              _onlied_ranks 
                = delete prev_guess 
                (onlyRank (map rank prev_guess) _next_remnants)
              -- only chooses the correct combination of suits.
              _onlied_suits 
                = delete prev_guess 
                (onlySuit (map suit prev_guess) _next_remnants)
