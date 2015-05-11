:- use_module(library(clpfd)).

queens(N, Pairs) :- n_vars(N, Pairs, Xs, Ys),
  all_distinct(Xs),
  all_distinct(Ys),
  append(Xs, Ys, Vars),
  Vars ins 1..N,
  no_diagonal_checks(Pairs),
  monotonic(Xs),
  label(Vars).

no_diagonal_checks(Pairs) :- no_diagonal_checks_left_down(Pairs), no_diagonal_checks_left_up(Pairs).
diff((X,Y), Z) :- Z #= X-Y.
sum((X,Y), Z) :- Z #= X+Y.

no_diagonal_checks_left_up(Pairs) :- maplist(diff, Pairs, Zs), all_distinct(Zs).
no_diagonal_checks_left_down(Pairs) :- maplist(sum, Pairs, Zs), all_distinct(Zs).

n_vars(0, [], [], []) :- !.
n_vars(N, [(X,Y)|T], [X|Xs], [Y|Ys]) :- N0 is N-1, n_vars(N0, T, Xs, Ys).

monotonic([_]) :- !.
monotonic([X,Y|Z]) :- X #=< Y, monotonic([Y|Z]).

% 1,1 2,1 3,1
% 1,2 2,2 3,2
% 1,3 2,3 3,3
%
% 0   1   2
% -1  0   1
% -2  -1  0
%
% 2 3 4
% 3 4 5
% 4 5 6
