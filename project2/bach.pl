% File: proj2.pl
% Author: Bach Tran.
% Student ID: 941113.
% Subject: COMP300020 Declarative Programming
% Coordinator: Peter Schatle.
% University of Melbourne, 2019.

:-use_module(library(apply)).
:-use_module(library(clpfd)).


full_of_digits(Matrix) :-
    Matrix = [[_|HeaderRow]|Puzzle_Rows],
    maplist(valid_header_elem, HeaderRow),
    maplist(valid_puzzle_row, Puzzle_Rows).
/**
 * A header element should equal to the sum or the
 * product of its corresponding column or row, also,
 * a column or row can only have distinct digits.
 * Therefore, its domain is 6..504 = 1*2*3..7*8*9.
 */
% TODO: strengthen this with stricter counting rules.
valid_header_elem(HeaderElem) :-
    HeaderElem #>= 1.

/**
 * Holds if the puzzle row is valid. A valid puzzle row
 * is a row starts with an element in the Header, and
 * the rest of it follow constraints 1 and 2.
 */
valid_puzzle_row([HeaderElem|Row]) :-
    valid_header_elem(HeaderElem),
    (
        sum(Row, #=, HeaderElem);
        product(Row, HeaderElem)
    ),
    Row ins 1..9,
    all_distinct(Row).


% Helper facilities

/**
 * Factorial function. Using tail recursive.
 */
factorial(0, 1).
factorial(N, F) :-
    N #> 0,
    N1 is N-1,
    F #= N*F1,
    factorial(N1, F1).
% TODO check the logic below:
/**
 * Calculate the permutations, i.e. the Answer
 * of the Questions: "How many choices we have,
 * if we select R from N"
 */
permutations(0, _, 0).
permutations(N, R, Permutations) :-
    N #>= R,
    R1 is (N-R+1),
    numlist(R1, N, List),
    product(List, Permutations).
/**
 * Calculates the product of a list of integers.
 * This function is supplied due to the
 * unavailability of an equivalence in clpfd.
 */
product([], 0).
product(List, Product) :- 
    product(List, 1, Product).
/**
 * Calculates the product of a list of integers,
 * using an Accumulator to achieve efficiency
 * of tail recursion.
 */
product([], Accumulator, Accumulator).
product([0|_], _, 0) :- !.
product([N|Ns], Accumulator, Product) :-
    A1 is Accumulator * N,
    product(Ns, A1, Product).