:- use_module(library(clpfd)).

sat(X) :- rewrite(X, P), term_variables(P, V), ins(V, 0 \/ 1), P #= 1, label(V).

rewrite(X, X) :- var(X), !.
rewrite(and(X, Y), XP*YP) :- !, rewrite(X, XP), rewrite(Y, YP).
rewrite(or(X, Y), XP+YP) :- !, rewrite(X, XP), rewrite(Y, YP).
rewrite(neg(X), (1-XP)) :- !, rewrite(X, XP).
