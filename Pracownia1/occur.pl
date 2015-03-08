% vi: syntax=prolog, filetype=prolog
occurences(S, T, N) :- occures(S, T, 0, N).
occures(S, S, A, B) :- B is A + 1, !.
occures(S, F, A, B) :- F =.. [_ | T], foldl_occures(S, T, A, B).
foldl_occures(_, [], A, A).
foldl_occures(S, [H|T], A, B) :- occures(S, H, A, Ap), foldl_occures(S, T, Ap, B).
