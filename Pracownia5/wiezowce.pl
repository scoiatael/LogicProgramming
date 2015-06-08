:- use_module(library(clpfd)).

% n_max([1,1,3,4,5,2,1], 4) % [1,3,4,5]
% n_max([1,4,1,5,2,6], 4) % [1,4,5,6]

calc_n_max([H|T], X) :- calc_n_max(T, H, X0), X is X0+1.
calc_n_max([], _, 0).
calc_n_max([H|T], Max, N) :- H =< Max, !, calc_n_max(T, Max, N).
calc_n_max([H|T], Max, N) :- H > Max, !, calc_n_max(T, H, N0), N is N0+1.

permi([], []).
permi([H|T], S) :-
  permi(T, S0),
  select(H, S, S0).

generate_n_max(N) :-
  findall(X, between(1, N, X), List),
  findall(_, (
    permi(List, Perm),
    calc_n_max(Perm, X),
    reverse(Perm, RPerm),
    calc_n_max(RPerm, Y),
    assert(n_max(Perm, (X, Y)))
  ), _).

wiezowce(N, Rows, Cols, Sq) :-
  generate_n_max(N),
  n_square(N, Sq),
  maplist(const_nil, Sq, KnownCols),
  constraints(KnownCols, Sq, Rows, Cols),
  retractall(n_max(_,_)).

const_nil(_, []).

n_square(N, Sq) :-
  length(Sq, N),
  maplist(length_flipped(N), Sq).

length_flipped(X, Y) :- length(Y, X).

zip([], [], []).
zip([H1|T1], [H2|T2], [(H1, H2) | T]) :- zip(T1, T2, T).

constraints(_, _, _, []) :- !.
% Cols are Row-list
% [X1, X2, X3] [X4, X5, X6]
% [Y1, Y2, Y3] [Y4, Y5, Y6]
% [Z1, Z2, Z3] [Z4, Z5, Z6]
constraints(KnownCols, UnknownCols, Rows, [C|Cols]) :-
  maplist(head, UnknownCols, Heads),
  maplist(tail, UnknownCols, Tails),
  generate_col(KnownCols, Heads, Rows, KnownCols1, Heads, C),
  constraints(KnownCols1, Tails, Rows, Cols).

generate_col([], [], [], [], _, _).
generate_col([Row|Rows], [H|T], [RDesc|RowDescs], [RWithH|RowsWithT], Heads, C) :-
  append(Row, [X|_], FullRow),
  findall(X, n_max(FullRow, RDesc), Xs),
  sort(Xs, XsS),
  member(H, XsS),
  append(Row, [H], RWithH),
  \+ \+ n_max(Heads, C),
  generate_col(Rows, T, RowDescs, RowsWithT, Heads, C).

head([H|_], H).
tail([_|T], T).

test1(8, [ (2, 4), (3, 2), (1, 4), (3, 1), (2, 4), (3, 3), (3, 2), (2, 3)], [ (2, 3), (3, 3), (4, 1), (1, 3), (2, 2), (3, 2), (4, 2), (3, 2)]).
test2(4, [ (2, 2), (1, 3), (2, 2), (3, 1)], [ (2, 2), (3, 2), (1, 2), (4, 1)]).
