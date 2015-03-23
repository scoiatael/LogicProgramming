% vi: syntax=prolog, filetype=prolog
monotonic([X,Y|T], [X|B], Z) :- X =< Y, !, monotonic([Y|T], B, Z).
monotonic([H|T], [H], T) :- !.

bubsort(L, Sorted) :- monotonic(L, B, S),
  ( S = [], !, Sorted = L
  ;
    S = [H|T],
    select_higher(H, X, B, [B1, B2]),
    append2([H,X], [B1, B2, T], L1),
    bubsort(L1, Sorted)
  ).

select2([A,B], [P1, P2, P3], L) :-
  select1(A, L, [P1, ARest]),
  \+ ARest = [],
  select_lower(A, B, ARest, [P2, P3]),
  A > B, !.
select1(A, L, [P1, P2]) :- append(Beg, P2, L), append(P1, [A], Beg).
select_lower(A, H, [H|T], [[], T]) :- H < A, !.
select_lower(A, C, [H|T], [[H|B], E]) :- select_lower(A, C, T, [B, E]).

select_higher(A, H, [H|T], [[], T]) :- H > A, !.
select_higher(A, C, [H|T], [[H|B], E]) :- select_higher(A, C, T, [B, E]).

append2([A,B], [P1, P2, P3], L) :- append(P1, [A|P2], Temp), append(Temp, [B|P3], L).
