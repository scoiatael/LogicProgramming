% vi: syntax=prolog, filetype=prolog

multiset([], void).
multiset([H|T], bag(H, Kr, R)) :- occures(H, [H|T], Kr,0, Tp), multiset(Tp, R).

occures(H, [H|T], KrP, Kr, Tp) :- !, KrPp is Kr + 1, occures(H, T, KrP, KrPp, Tp).
occures(H, [X|T], KrP, Kr, [X|Tp]) :- occures(H, T, KrP, Kr, Tp).
occures(_, [], A, A, []).
