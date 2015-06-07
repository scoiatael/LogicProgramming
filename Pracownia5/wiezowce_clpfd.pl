:- use_module(library(clpfd)).
% n_max([1,1,3,4,5,2,1], 4) % [1,3,4,5]
% n_max([1,4,1,5,2,6], 4) % [1,4,5,6]

n_max([H|T], X) :- n_max(T, H, X0), X #= X0+1.
n_max([], _, 0).
n_max([H|T], Max, N) :-
  H #> Max #==> N #= N0+1,
  H #=< Max #==>  N #= N0, !, n_max(T, max(H, Max), N0).

wiezowce(N, Rows, Cols, Sq) :-
  n_square(N, Sq),
  flatten(Sq, Vars),
  Vars ins 1..N,
  row_constraints(Sq, Rows),
  transpose(Sq, TSq),
  row_constraints(TSq, Cols),
  label(Sq).

n_square(N, Sq) :-
  length(Sq, N),
  maplist(length_flipped(N), Sq).

length_flipped(X, Y) :- length(Y, X).

zip([], [], []).
zip([H1|T1], [H2|T2], [(H1, H2) | T]) :- zip(T1, T2, T).

row_constraints(Sq, Rows) :-
  zip(Rows, Sq, RowDescs),
  maplist(row_constraint, RowDescs).

row_constraint((RowDesc, Row)) :-
  n_max(Row, RowDesc),
  all_different(Row).
