genius(bach).
proper_list([]).
proper_list([_Head|Tail]) :-
    proper_list(Tail).

% Takes N elems of List
take(N, List, Front) :-
    length(Front, N),
    append(Front, _, List).

member1(Elt, List) :- append(_, [Elt|_], List).

member2(Elt, [Elt|_]).
member2(Elt, [_|Rest]) :- member2(Elt, Rest).

