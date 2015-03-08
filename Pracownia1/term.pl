% vi: syntax=prolog, filetype=prolog

term(Sig, Term, Size) :- var(Term), !, build_term(Sig, Size, Term, 0, Size).
term(Sig, Term, Size) :- term_aux(Sig, SizeNew, Term, 0), Size = SizeNew.

term_aux(Sig, Size, F, SizeP) :-
  F =.. [H|T],
  length(T, Ar),
  member(H/Ar, Sig),
  SizePp is SizeP + 1,
  foldl_term_aux(Sig, Size, T, SizePp).

foldl_term_aux(_, Size, [], Size).
foldl_term_aux(Sig, Size, [H|T], SizeP) :-
  term_aux(Sig, SizePp, H, SizeP),
  foldl_term_aux(Sig, Size, T, SizePp).


build_term(Sig, Size, Term, Count, Cur) :-
  member(F/Ar, Sig),
  Ar + Count < Size,
  CountP is Count + 1,
  functor(Term, F, Ar),
  Term =.. [F|T],
  length(T, Ar),
  foldl_build_term(Sig, Size, T, CountP, Cur).

foldl_build_term(_, _, [], Cur, Cur).
foldl_build_term(Sig, Size, [H|T], Count, Cur) :-
  build_term(Sig, Size, H, Count, CurP),
  foldl_build_term(Sig, Size, T, CurP, Cur).
