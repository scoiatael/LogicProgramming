resolution(Clauses, Proof):-
  prepare(Clauses, PClauses),
  prove(PClauses, Proof), !.

prove(Clauses, Proof):-
  member(([], Proof), Clauses), !.

prove([H|T], Proof):-
  H = ([P|_], _),
  divide(P, [H|T], WithP, NegP, NoP),
  resolve(P, WithP, NegP, Res),
  append(NoP, Res, NewSet),
  prove(NewSet, Proof).

resolve(P, L1, L2, Res):-
  findall(M, (member(H1, L1), member(H2, L2), merge(H1, H2, P, M), M = (C,_), \+tautology(C)), Res).

negation(- Q, Q) :-!.
negation(Q, -Q).

merge((C1, P1), (C2, P2), Q, (S, resolvent(S, P1, P2))):-
  select(Q, C1, NC1),
  negation(Q, NQ),
  select(NQ, C2, NC2), !,
  append(NC1, NC2, R),
  sort(R, S).

divide(P, List, WithP, WithNP, WithoutP) :-
  findall((HP, Proof), ( member((HP, Proof), List), hasP(P, HP)), WithP),
  findall((HNP, Proof),( member((HNP, Proof), List), hasNP(P, HNP)), WithNP),
  findall((NoP, Proof),( member((NoP, Proof), List), noP(P, NoP)), WithoutP).

hasP(P, Cl) :- member(P, Cl).
hasNP(P, Cl) :- member(-P, Cl) ; NP is -1 * P, member(NP, Cl).
noP(P, Cl) :- \+ hasP(P, Cl), \+ hasNP(P, Cl).

prepare(X, Y) :-
  findall((SCl, axiom(SCl)), (member(Cl, X),\+ tautology(Cl), sort(Cl, SCl)), NTaus),
  sort(NTaus, Y).

tautology(L):-
  member(X, L),
  negation(X, Y),
  member(Y, L).
