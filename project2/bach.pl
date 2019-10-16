% File: proj2.pl
% Author: Bach Tran.
% Student ID: 941113.
% Subject: COMP300020 Declarative Programming
% Coordinator: Peter Schatle.
% University of Melbourne, 2019.

:-use_module(library(apply)).
:-use_module(library(clpfd)).

puzzle_solution(Puzzle) :-
    uniform_diagonal(Puzzle),
    valid_rowcol_digits(Puzzle).

%%%%%%%% ROW-COLUMN SECTION %%%%%%%%

/**
 * Ignores the first row, which is assumed to be
 * the Header.
 */
valid_rowcol_digits(Matrix) :-
    Matrix = [_|Puzzle_Rows],
    transpose(Matrix, MatrixT),
    MatrixT = [_|Puzzle_Rows_T],
    maplist(valid_puzzle_row, Puzzle_Rows),
    maplist(valid_puzzle_row, Puzzle_Rows_T),
    maplist(label, Puzzle_Rows).
/**
 * A header element should equal to the sum or the
 * product of its corresponding column or row, also,
 * a column or row can only have distinct digits.
 * Therefore, its domain is 6..504 = 1*2*3..7*8*9.
 */
valid_header_elem(HeaderElem, BoardSize) :-
    sum_1n(BoardSize, LowerBound),
    permutations(9, BoardSize, UpperBound),
    between(LowerBound, UpperBound, HeaderElem).

/**
 * Holds if the puzzle row is valid. A valid puzzle row
 * is a row starts with an element in the Header, and
 * the rest of it follow constraints 1 and 2.
 */
valid_puzzle_row([HeaderElem|Row]) :-
    length(Row, RowLength),
    valid_header_elem(HeaderElem, RowLength),
    Row ins 1..9,
    all_distinct(Row),
    (
        sum(Row, #=, HeaderElem)
        ;
        product(Row, 1 , HeaderElem)
    ).

%%%%%%%% DIAGONAL SECTION %%%%%%%%
uniform_diagonal(Puzzle) :-
    diagonal(Puzzle, [_|Ds]),
    uniform(Ds).

square(Matrix) :- maplist(same_length(Matrix), Matrix).

diagonal(Matrix, Diagonal) :-
    square(Matrix),
    diagonal(Matrix, 1, Diagonal).

diagonal([],_,[]).
diagonal([Row|Rows], Index, [D|Ds]) :-
    nth1(Index, Row, D),
    NextIndex #= Index+1,
    diagonal(Rows, NextIndex, Ds).

uniform([]).
uniform([X|Xs]) :- uniform(Xs, X).

uniform([], _).
uniform([X|Xs], X) :- uniform(Xs, X).

sum_1n(N, Sum) :-
    N #>= 1,
    numlist(1, N, Numlist),
    sumlist(Numlist, Sum).

/**
 * Calculate the permutations, i.e. the Answer
 * of the Questions: "How many choices we have,
 * if we select R from N"
 */
permutations(0, _, 0).
permutations(N, R, Permutations) :-
    N #>= R, N #> 0, R #> 0,
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
    A1 #= Accumulator * N,
    product(Ns, A1, Product).