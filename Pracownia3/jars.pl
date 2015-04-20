jars(V, T, K) :-
  assertz(visited(_):-  fail),
  add_zeroes(V, Vp),
  (
    bfs_moves(T, K, [(0,Vp)|X]-X), !, retractall(visited(_))
  ; retractall(visited(_)), fail
  ).

move(S, Sn) :- fill_a_jar(S, Sn) ; empty_a_jar(S, Sn) ; transfer_with_overflow(S, Sn) ; transfer_without_overflow(S, Sn).

fill_a_jar([(X, C)|L], [(C, C) | L]) :- X < C.
fill_a_jar([H|L], [H|Lp]) :- fill_a_jar(L, Lp).

empty_a_jar([(X, C)|L], [(0, C) | L]) :- X > 0.
empty_a_jar([H|L], [H|Lp]) :- empty_a_jar(L, Lp).

transfer_with_overflow(L1, L2) :-
  select2(X, Y, L1, L1p),
  (overflow_from(X, Y, Xp, Yp) ;  overflow_from(Y, X, Yp, Xp)),
  append2(Xp, Yp, L2, L1p).

overflow_from((XV, XM), (YV, YM), (Xpp, XM), (Ypp, YM)) :-
  XV < XM, YV > 0,
  Xp is XV+YV, Ypp = 0,
  truncate_value(Xp, XM, Xpp).

transfer_without_overflow(L1, L2) :-
  select2(X, Y, L1, L1p),
  (flow_from(X, Y, Xp, Yp) ; flow_from(Y, X, Yp, Xp)),
  append2(Xp, Yp, L2, L1p).

flow_from((XV, XM), (YV, YM),(Xpp, XM), (Ypp, YM)) :-
  XV < XM, YV > 0,
  Xp is XV+YV, overflow(Xp, XM, (Xpp, Ypp)).

truncate_value(X, _, 0) :- X =< 0, !.
truncate_value(X, Max, Max) :- X >= Max, !.
truncate_value(X, _, X).

overflow(X, Max, (X, 0)) :- X =< Max, !.
overflow(X, Max, (Max, R)) :- R is X - Max.

select2(P1, P2, L, (A, B, C)) :- append(A, [P1|As], L), append(B, [P2|C], As).
append2(P1, P2, L, (A, B, C)) :- append(A, [P1|B], As), append(As, [P2|C], L).

add_zeroes([], []) :- !.
add_zeroes([X|Xs], [(0, X) | XZs]) :- add_zeroes(Xs, XZs).

bfs_moves(T, K, [(K,S)|_]-_) :- member((T, _), S), !.
bfs_moves(T, Kp, [(K, S)|N]-Ts) :-
  \+ visited(S), assert(visited(S)), !,
  Kn is K + 1,
  findall((Kn, Sn), move(S, Sn), Ls), append(Ls, Tsp, Ts),
  bfs_moves(T, Kp, N-Tsp).
bfs_moves(T, Kp, [_|N]-Ts) :- \+ var(N), bfs_moves(T, Kp, N-Ts).
