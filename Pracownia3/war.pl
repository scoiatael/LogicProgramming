war(L1, L2, N) :- assertz(was_here(_, _) :- fail), war(L1, L2, 0, C, []), C = N, retractall(was_here(_, _)).

war(X, Y, C, C, _) :- ends(X, Y), !.
war(H1, H2, _, inf, _) :- was_here(H1, H2), !.
war([H1|L1], [H2|L2], C, N, Ca) :- asserta(was_here([H1|L1], [H2|L2])), inc(C, Cp), push_stack(H1, H2, Ca, CaN),
  ( H1 = H2, !, draw_step(L1, L2, Cp, N, CaN)
  ; H1 < H2, !, append_stacks(CaN, L2, L2p), war(L1, L2p, Cp, N, [])
  ; H1 > H2, !, append_stacks(CaN, L1, L1p), war(L1p, L2, Cp, N, [])
  ).

draw_step(L1, L2, C, C, _) :- ends(L1, L2), !.
draw_step([H1|L1], [H2|L2], C, N, Ca) :- inc(C, Cp), push_stack(H1, H2, Ca, CaN), war(L1, L2, Cp, N, CaN).

push_stack(H1, H2, Ca, [H2, H1 | Ca]).

ends([], _).
ends(_, []).

append_stacks(S1, S2, ES) :- reverse(S1, S1R), append(S2, S1R, ES).

inc(C, Cp) :- Cp is C+1.
