% vi: syntax=prolog, filetype=prolog
bubsort(L, Sorted) :- select2([A,B], Parts, L), append2([B,A], Parts, L1), bubsort(L1, Sorted).
bubsort(S, S).

select2([A,B], [P1, P2, P3], L) :-
  select1(A, L, [P1, ARest]),
  \+ ARest = [],
  select1(B, ARest, [P2, P3]),
  A > B, !.
select1(A, L, [P1, P2]) :- append(Beg, P2, L), append(P1, [A], Beg).

append2([A,B], [P1, P2, P3], L) :- append(P1, [A|P2], Temp), append(Temp, [B|P3], L).
