wygrywa(L, K):-
  assertz(wins(_) :- fail),
  findall(X, between(0, K, X), Dom),
  reverse(Dom, RDom),
  maplist(wins(L, K), RDom, _),
  wins(0), !,
  retractall(wins(X)).

wins(L, K, N, 1) :-
  ( wins(L, K, N), !, asserta(wins(N))
  ; true
  ).

wins(L, K, N) :- findall(X, (member(X, L), Xp is N+X, (Xp >= K ; \+ wins(Xp))), Xs), Xs \= [].
