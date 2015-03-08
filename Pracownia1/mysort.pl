% vi: syntax=prolog, filetype=prolog

mysort(List, ListSorted) :- merge_sort(List, ListSorted).

merge_sort([], []).
merge_sort([X], [X]) :- !.
merge_sort(T, Sorted) :- split(T, T1, T2), merge_sort(T1, T1s), merge_sort(T2, T2s), merge(T1s,T2s,Sorted).

split(L, L1, L2) :- length(L, Le), Le2 is Le / 2, split(L, L1, L2, Le, Le2).
split(L, [], L, Le, Le2) :- Le2 >= Le, !.
split([H|L], [H|L1], L2, Le, Le2) :- Lep is Le - 1, split(L, L1, L2, Lep, Le2).

merge([], X, X) :- !.
merge(X, [], X) :- !.
merge([X|Xs], [Y|Ys], [X|T]) :- X =< Y, !, merge(Xs, [Y|Ys], T).
merge([X|Xs], [Y|Ys], [Y|T]) :- merge([X|Xs], Ys, T).
