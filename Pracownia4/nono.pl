:- use_module(library(clpfd)).

nono(Rzedy, Kolumny, B) :-
  length(Rzedy, NRzedow),
  length(Kolumny, NKolumn),
  length(B, NRzedow),
  maplist(length_swapped(NKolumn), B),
  flatten(B, Vars),
  Vars ins 0 \/ 1,
  transpose(B, BTransp),
  constraint_rows(B, Rzedy),
  constraint_rows(BTransp, Kolumny),
  labeling([ff, bisect], Vars).

constraint_rows(B, Rows) :-
  zip(B, Rows, Zipped),
  maplist(constraint_rows_aux, Zipped).

constraint_rows_aux((Row, RowDetail)) :-
  constraint_detail(RowDetail, 1, 0, RowCLPFD),
  constraint_row(Row, 1, 0, 0, RowCLPFD).

constraint_row([], _, _, Acc, Row) :- Acc #= Row.
constraint_row([H|T], PowerOfTwo, PrevH, Acc, Row) :-
  %fd_sup(PowerOfTwo, SupPower), SupPower1 is 2*SupPower+1, PowerOfTwo1 in 1..SupPower1,
  %fd_inf(Acc, InfAcc1), SupAcc1 is Row+1, Acc1 in InfAcc1..SupAcc1,
    PowerOfTwo1 = PowerOfTwo + PowerOfTwo*max(H, PrevH)
  , Acc1 = Acc + PowerOfTwo*H
  , constraint_row(T, PowerOfTwo1, H, Acc1, Row).

constraint_detail([], _, Acc, Acc) :- !.
constraint_detail([H|T], PowerOfTwo, Acc, Row) :-
  increment(H, PowerOfTwo, Acc, PowerOfTwo1, Acc1),
  PowerOfTwo0 is PowerOfTwo1*2,
  constraint_detail(T, PowerOfTwo0, Acc1, Row).

increment(0, PowerOfTwo, Acc, PowerOfTwo, Acc) :- !.
increment(X, PowerOfTwo, Acc, PowerOfTwo0, Acc0) :-
  X > 0, !, X1 is X - 1,
  PowerOfTwo1 is PowerOfTwo * 2,
  Acc1 is Acc + PowerOfTwo,
  increment(X1, PowerOfTwo1, Acc1, PowerOfTwo0, Acc0).

length_swapped(N, T) :- length(T, N).

zip([], [], []).
zip([H1|T1], [H2|T2], [(H1,H2)|T3]) :- zip(T1, T2, T3).

test1(
    [[4],[1,1,6],[1,1,6],[1,1,6],[4,9],[1,1],[1,1],[2,7,2],[1,1,1,1],[2,2]],
    [[4],[1,2],[1,1],[5,1],[1,2],[1,1],[5,1],[1,1],[4,1],[4,1],[4,2],[4,1],[4,1],[4,2],[4]]
).
