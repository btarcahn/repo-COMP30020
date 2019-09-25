% Tail recursion
sum_list([N|Ns], A, S) :-
    A0 is A + N.
    sum_list(Ns, A0, S).

sum_list(Ns, Sum) :- sum_list(Ns, 0, Sum).


tree_list(empty, []).
tree_list(node(L, V, R), List) :-
    tree_list(L, LL),
    tree_list(R, RL),
    append(LL, [V|RL], List).

tree_list(empty, A, A).
tree_list(node(LT, V, RT), A, A2) :-
    tree_list(RT, A, A1),
    tree_list(LT, [V|A1, A2]),
tree_list(T, List) :-
    tree(T),
    tree_list(T, [], List).

list_tree([], empty).
list_tree([H|Tail], B) :-
    length([H|Tail], L),
    length(Front,M), M is L // 2,
    list_tree(Front, Left), append(Front, [V | Back], [H| Tail]),
    list_tree(Back, Right), B = node(Front, V, Back).