% vi: syntax=prolog, filetype=prolog

connected(X,Y) :- connected(X, Y, []).

connected(X, X, _).
connected(X,Y, Li) :- edge(X, W), \+ member(W, Li), connected(W, Y, [X|Li]).
