% Prolog stuffs
% Q1
same_elements(List1, List2) :-
    subset(List1, List2), subset(List2, List1).

subset([], _).
subset([X|XS], L) :- member(X, L), subset(XS, L).
subset([], _).

% sort
% msort
% keysort: [k-v]

same_elements(List1, List2) :-
    sort(List1, Sorted1), sort(List2, Sorted2),
    list_eq(Sorted1, Sorted2).

list_eq([], []).
list_eq([X|XS], [X, YS]) :- list_eq(XS, YS).

% Q3
% integers |variables
% W*X+Y=Z
% 0 <= Y < |W|
% W -> INT, at least one of X and X -> INT

times(W, X, Y, Z) :-
    integer(W), integer(X),
    Yup is abs(W)-1, between(0, Yup, Y),
    Z is W*X+Y.
times(W, X, Y, Z) :-
    integer(W), integer(Z),
    Yup is abs(W)-1, between(0, Yup, Y),
    X is (Z-Y)/W.
times(W, X, Y, Z) :-
    integer(W), integer(Z),
    Yup is abs(W)-1, between(0, Yup, Y),
    X is (Z-Y)/W.

% Q4

take_action([], Goal, _, _, Goal) :- !.
take_action([Action|Rest], State0, State, Hist, Goal) :-
    effect(Action, State0, State),
    not(member(State, Hist)),
    take_action(Rest, State, _, [State|Hist], Goal).

effect(fill(5), container(X, Y), container(5, Y)) :-
    X =\= 5.
effect(fill(3), container(X, Y), container(X, 3)) :-
    Y =\= 3.
effect(empty(5), container(X, Y), container(0, Y)) :-
    X =\= 0.
effect(empty(3), container(X, Y), container(X, 0)) :-
    Y =\= 0.
effect(pour(5, 3), container(X, Y), container(0, Sum_xy)) :-
    X > 0, Sum_xy is X+Y, X + Y =< 3.
effect(pour(5, 3), container(X, Y), container(X1, 3)) :-
    X > 0, X1 is X+Y-3, X1 > 0.
effect(pour(3, 5), container(X, Y), container(Sum_XY, 0)) :-
    Y > 0, Sum_XY is X+Y, Sum_XY =< 5.
effect(pour(3, 5), container(X, Y), container(5, Y1)) :-
    Y > 0, Y1 is X+Y-5, Y1 > 0.

