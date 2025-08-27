sublist(L, M) :-
    append(_, T, L),
    append(M, _, T),
    \+append(M, L, L).

subseq(_, []). 
subseq([H|L], [H|M]) :-
    subseq(L, M).
subseq([_|L], M) :-
    subseq(L, M).

disjoint([], [], []).
disjoint([H|L], M, [H|N]) :-
    disjoint(L, M, N).
disjoint([H|L], [H|M], N) :-
    disjoint(L, M, N).