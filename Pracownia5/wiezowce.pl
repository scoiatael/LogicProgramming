% n_max([1,1,3,4,5,2,1], 4) % [1,3,4,5]
% n_max([1,4,1,5,2,6], 4) % [1,4,5,6]

calc_n_max([H|T], X) :- n_max(T, H, X0), X is X0+1.
calc_n_max([], _, 0).
calc_n_max([H|T], Max, N) :- H =< Max, !, n_max(T, Max, N).
calc_n_max([H|T], Max, N) :- H > Max, !, n_max(T, Max, N0), N is N0+1.

permi([], [[]]).
permi([H|T], S) :-
  permi(T, S0),
  select(H, S, S0).

generate_n_max(N) :-
  findall(X, between(1, N, X), List),
  findall(_, (permi(List, Perm), calc_n_max(Perm, X), assert(n_max(Perm, X))), _).

wiezowce(N, Rows, Cols, Sq) :-
  generate_n_max(N),
  n_square(N, Sq),
  row_constraints(Sq, Rows),
  transpose(Sq, TSq),
  row_constraints(TSq, Cols),
  retractall(n_max(_,_)).

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
  n_max(Row, RowDesc).
