% vi: syntax=prolog, filetype=prolog

convert(A,B) :- msort(A,Ap), multisetP(Ap, B).

multisetP([], void).
multisetP([H|T], bag(H, Kr, R)) :- occures(H, [H|T], Kr,0, Tp), multisetP(Tp, R).

occures(H, [H|T], KrP, Kr, Tp) :- !, KrPp is Kr + 1, occures(H, T, KrP, KrPp, Tp).
occures(_, T, A, A, T).
