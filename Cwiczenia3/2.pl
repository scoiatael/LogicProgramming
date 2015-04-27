:- dynamic(has_value/2).

set(X, V) :- asserta(has_value(X, V)).
set(X, V) :- retract(has_value(X, V)), !, fail.

get(X, V) :- has_value(X, V), !.
