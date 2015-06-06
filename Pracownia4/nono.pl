:- use_module(library(clpfd)).

nono(Rzedy, Kolumny, B) :-
  length(Rzedy, NRzedow),
  length(Kolumny, NKolumn),
  length(B, NRzedow),
  maplist(B, length_swapped(NKolumn)),
  flatten(B, Vars),
  Vars ins 0 \/ 1,
  transpose(B, BTransp),
  constraint_rows(B, Rzedy),
  constraint_rows(BTransp, Kolumny),
  labeling([ff, bisect], Vars).

length_swapped(N, T) :- length(T, N).

constraint_rows(B, Rows) :-
  zip(B, Rows, Zipped),
  maplist(Zipped, constraint_rows_aux).

constraint_rows_aux(Row, RowDetail) :-
  sumlist(RowDetail, S),
  sum(Row, #=, S).

zip([], [], []).
zip([H1|T1], [H2|T2], [(H1,H2)|T3]) :- zip(T1, T2, T3).
