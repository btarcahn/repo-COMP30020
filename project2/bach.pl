% File: proj2.pl
% Author: Bach Tran.
% Purpose: validating & solving a Math Puzzle
% Subject: COMP300020 Declarative Programming
% Coordinator: Peter Schatle.
% University of Melbourne, 2019.

/**
 * ------------------- ABSTRACT ------------------
 * A valid Math Puzzle is a square of N*N
 * satisfies the following criteria:
 * 1. All elements in that square are digits (1-9)
 * 2. Each row or column contains distinct digits,
 * and the product or sum of those digits equals
 * to the corresponding Heading
 * 3. The diagonal line of the Puzzle has
 * identical numbers
 * For more information, see the assignment specs.
 *
 * We treat the criteria above as constraints, or
 * requirements of a valid Puzzle. The problem can
 * then be solved using Prolog's constraint
 * programming, which is quite powerful. The
 * puzzle_solution() predicate holds if the given
 * Puzzle is a valid solution, it works both as
 * a Puzzle validator and a Puzzle solver.
 * Under this predicate, the problem is further
 * broken down into sub-constraints, which are
 * presented in different sections below.
 * 
 * Note: this program also has some useful Prolog
 * predicates that can be applied for generic
 * problems beyond the scope of this Puzzle game.
 */

:-use_module(library(clpfd)).


%%%%%%%%%%%%%% PUZZLE SOLUTION %%%%%%%%%%%%%

/**
 * puzzle_solution(++Puzzle) is semidet
 * puzzle_solution(+Puzzle) is nondet
 * puzzle_solution(?Puzzle) is nondet
 * 
 * If ground: checks the if the given square Puzzle is
 * a valid solution for the Puzzle game
 * Otherwise: try to solve the Puzzle, using the
 * game rule, by determining unbounded variables.
 */
puzzle_solution(Puzzle) :-
    uniform_diagonal(Puzzle),
    valid_rowcol_digits(Puzzle).


%%%%%%%%%%%% ROW-COLUMN SECTION %%%%%%%%%%%%

/**
 * valid_rowcol_digits(++Puzzle) is semidet
 * valid_rowcol_digits(?Puzzle) is nondet
 * 
 * Holds if the each row and column of the square
 * Puzzle satisfies:
 * 1. Have distinct digits (1-9)
 * 2. Have valid header.
 * 3. The sum or product is equal to the Header.
 */
valid_rowcol_digits(Puzzle) :-
    Puzzle = [_|Puzzle_Rows],
    maplist(valid_puzzle_row, Puzzle_Rows),
    transpose(Puzzle, PuzzleT),
    PuzzleT = [_|Puzzle_Rows_T],
    maplist(valid_puzzle_row, Puzzle_Rows_T),
    maplist(label, Puzzle_Rows).

/**
 * valid_header_elem(++HeaderElem:int, ++BoardSize:int) is semidet
 * valid_header_elem(?HeaderElem:int, ?BoardSize:int) is nondet
 * 
 * Due to the constraint: "each row or column only
 * contains distinct digits", a valid HeaderElem 
 * is an integer in between the (Sum of 1 to the
 * BoardSize) and the Permutations(9, Boardsize).
 * Otherwise, given an invalid HeaderElem, we know
 * immediately that the Puzzle is not solvable.
 */
valid_header_elem(HeaderElem, BoardSize) :-
    sum_1n(BoardSize, LowerBound),
    permutations(9, BoardSize, UpperBound),
    between(LowerBound, UpperBound, HeaderElem).

/**
 * valid_puzzle_row(++Row) is semidet
 * valid_puzzle_row(+Row) is nondet
 * 
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


%%%%%%%%%%%%% DIAGONAL SECTION %%%%%%%%%%%%%
/**
 * uniform_diagonal(++Puzzle) is semidet
 * uniform_diagonal(+Puzzle) is nondet
 * uniform_diagonal(?Puzzle) is nondet
 * 
 * Holds if the diagonal line of a square Puzzle
 * is uniform.
 */
uniform_diagonal(Puzzle) :-
    diagonal(Puzzle, [_|Ds]),
    uniform(Ds).

%%%%%%%%% MATRIX SECTION (GENERIC) %%%%%%%%%
% NOTE: these predicates may be applicable %
% outside the scope of this assignment.    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**
 * square(++Matrix) is semidet
 * square(+Matrix) is semidet
 * 
 * Holds if Matrix (in form of a ListofLists),
 * is a square, i.e. size of NxN.
 */
square(Matrix) :- maplist(same_length(Matrix), Matrix).

/**
 * diagonal(+Matrix, -Diagonal) is semidet
 * 
 * Requirement: Matrix must be square (NxN),
 * if requirement is fulfilled, this predicate is
 * equivalent to diagonal(Matrix, 1, Diagonal).
 */
diagonal(Matrix, Diagonal) :-
    square(Matrix),
    diagonal(Matrix, 1, Diagonal).

/**
 * diagonal(+Matrix, ++Index, -Diagonal) is det
 * 
 * Given a 2D array (a ListOfLists), extracts
 * its Diagonal elements, starting at Index of
 * the first row.
 */
diagonal([],_,[]).
diagonal([Row|Rows], Index, [D|Ds]) :-
    nth1(Index, Row, D),
    NextIndex #= Index+1,
    diagonal(Rows, NextIndex, Ds).

%%%%%%%%%% LIST SECTION (GENERIC) %%%%%%%%%%
% NOTE: these predicates may be applicable %
% outside the scope of this assignment.    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**
 * uniform(++List) is semidet
 * uniform(+List) is det
 * uniform(?List) is nondet
 * 
 * Equivalent to uniform([X|Xs], X).
 */
uniform([]).
uniform([X|Xs]) :- uniform(Xs, X).

/**
 * uniform(+List, +Value) is semidet
 * uniform(?List, ?Value) is nondet
 * 
 * Holds if all elements in the List are identical
 * to Value.
 */
uniform([], _).
uniform([X|Xs], X) :- uniform(Xs, X).

/**
 * sum_1n(++N:int, -Sum:int) is semidet
 * 
 * Sum is the result of adding all positive integers
 * from 1 to N, i.e.
 * Sum = 1 + 2 + 3 +... + N-1 + N.
 * Otherwise, if N < 1, the predicate fails.
 */
sum_1n(N, Sum) :-
    N #>= 1,
    numlist(1, N, Numlist),
    sumlist(Numlist, Sum).

/**
 * permutations(++N:int, ++R:int, Permutations:int) is semidet
 * 
 * Permutations is the answer to the question: 
 * "How many choices we have, if we select
 * R samples from N objects",
 * If N >= R > 0, the following formula
 * is applied:
 * P(N,R) = N! / (N-R)!
 * Otherwise, it fails.
 */
permutations(0, _, 0) :- !.
permutations(_, 0, 0) :- !.
permutations(N, R, Permutations) :-
    N #>= R, N #> 0, R #> 0,
    R1 is (N-R+1),
    numlist(R1, N, List),
    product(List, Permutations).

/**
 * product(++List:ints, -Product:int) is det
 * 
 * Equivalent to product(List, 1, Product).
 */
product([], 0).
product(List, Product) :- 
    product(List, 1, Product).
/**
 * product(++List, ++Accumulator, -Product) is det
 * 
 * Product is the product of all elements given
 * in a List, preferably a List of integers or
 * decimals. The predicate requires an accumulator,
 * generally be 1, to do the calculation.
 */
product([], Accumulator, Accumulator).
product([0|_], _, 0) :- !.
product([N|Ns], Accumulator, Product) :-
    A1 #= Accumulator * N,
    product(Ns, A1, Product).