connected(X, Y):- bfs_moves(Y, [X], []).

bfs_moves(Y, [Y|_], _) :- !.
bfs_moves(Y, [H|T], Visited) :- member(H, Visited), !, bfs_moves(Y, T, Visited).
bfs_moves(Y, [H|T], Visited) :- findall(Z, edge(H, Z), Zs), append(T, Zs, Ts), bfs_moves(Y, Ts, [H|Visited]).
