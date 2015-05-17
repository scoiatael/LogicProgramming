:- use_module(library(clpfd)).

queens(N, Xs) :- length(Xs, N),
  add_Ys(0, Xs, Pairs),
  Xs ins 1..N,
  all_distinct(Xs),
  no_diagonal_checks(Pairs),
  labeling([ff, bisect], Xs).

no_diagonal_checks([_]) :- !.
no_diagonal_checks([Q|Qs]) :- no_diagonal_checks_left_down(Q, Qs), no_diagonal_checks_left_up(Q, Qs), no_diagonal_checks(Qs).

no_diagonal_checks_left_up(Q, Qs) :- maplist(diff(Q), Qs).
no_diagonal_checks_left_down(Q, Qs) :- maplist(sum(Q), Qs).

diff((X,Y), (A,B)) :- X-Y #\= A-B.
sum((X,Y), (A,B)) :- X+Y #\= A+B.

add_Ys(_, [], []) :- !.
add_Ys(N, [X|Xs], [(X,N)|Pairs]) :- N0 is N+1, add_Ys(N0, Xs, Pairs).

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
