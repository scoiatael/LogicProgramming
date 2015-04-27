%vi syntax: prolog
create([undef | _]).

get(Var, Val) :- aux(Var, Val, _).
set(Var, Val) :- aux(Var, _, Tail), Tail = [Val | _].

aux([H|T], H, T) :- var(T).
aux([_|T], Val, Tail) :- \+ var(T), aux(T, Val, Tail).
