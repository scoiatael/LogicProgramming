lookup(K, (K, V1, _, _), V) :-
    V = V1 -> !, true
  ; !, fail.

lookup(K, (C, _, L, R), V) :-
    K < C -> lookup(K, L, V)
  ; lookup(K, R, V).
