% stan:
% Kapusta Koza Wilk Lodka
% 0 - na lewym brzegu
% 1 - na prawym brzegu
forbidden(X-X-_-Y) :- X \= Y.
forbidden(_-X-X-Y) :- X \= Y.

new_state(0-0-0-0).
final_state(1-1-1-1).

play(T) :- new_state(S), final_state(F), connected(S, F, T).

edge(X, Y):- move(X, Y), \+ forbidden(Y).

move(A-B-C-D, Ap-Bp-Cp-Dp) :- invert(D, Dp),
  (  A = D, invert(A, Ap), Bp = B, Cp = C
  ;  B = D, invert(B, Bp), Ap = A, Cp = C
  ;  C = D, invert(C, Cp), Ap = A, Bp = B
  ;  Ap = A, Bp = B, Cp =C
  ).

invert(1, 0).
invert(0, 1).

connected(X, Y, Hist):- bfs_moves(Y, [X-[]], [], Hist).

bfs_moves(Y, [Y-Hi|_], _, Hi) :- !.
bfs_moves(Y, [H-_|T], Visited, Hi) :- member(H, Visited), !, bfs_moves(Y, T, Visited, Hi).
bfs_moves(Y, [H-Hist|T], Visited, Hi) :- findall(Z, edge(H, Z), Zs), append_history(Zs, [H|Hist], Zss), append(T, Zss, Ts), bfs_moves(Y, Ts, [H|Visited], Hi).

append_history([], _, []).
append_history([H|T], Hi, [H-Hi|Tp]) :- append_history(T, Hi, Tp).
