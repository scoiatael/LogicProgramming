:- use_module(library(clpfd)), dynamic(hash/3).

generate_possibilities(_, []).
generate_possibilities(N, [H|T]) :-
  length(Li, N),
  Li ins 0 \/ 1,
  %row_hash(H, Hash),
  findall(_, (insert_ones(Li), constraint_row(Li, Hash), \+ hash(N, Hash, Li), asserta(hash(N, Hash, Li))), _).
  %findall(_, (label(Li), \+ hash(N, Hash, Li), asserta(hash(N, Hash, Li))), _),
  %generate_possibilities(N, T).

insert_ones([]).
insert_ones([H|T]) :-
  member(H, [0,1]),
  insert_ones(T).

nono(Rzedy, Kolumny, B) :-
  length(Rzedy, NRzedow),
  length(Kolumny, NKolumn),
  generate_possibilities(NKolumn, Kolumny),
  generate_possibilities(NRzedow, Rzedy),
  length(B, NRzedow),
  maplist(length_swapped(NKolumn), B),
  flatten(B, Vars),
  Vars ins 0 \/ 1,
  transpose(B, BTransp),
  constraint_rows_by_possibilities(B, Rzedy, NKolumn),
  constraint_rows(B, Rzedy),
  constraint_rows_by_possibilities(BTransp, Kolumny, NRzedow),
  constraint_rows(BTransp, Kolumny),
  labeling([ffc, enum], Vars).

constraint_rows_by_possibilities(B, Rows, NCol) :-
  zip(B, Rows, Zipped),
  maplist(constraint_rows_by_possibilities_aux(NCol), Zipped).

constraint_rows_by_possibilities_aux(N, (Row, RowDetail)) :-
  row_hash(RowDetail, RowHash),
  findall(Li, hash(N, RowHash, Li), Poss),
  tuples_in([Row], Poss).

constraint_rows(B, Rows) :-
  zip(B, Rows, Zipped),
  maplist(constraint_rows_aux, Zipped).

constraint_rows_aux((Row, RowDetail)) :-
  row_hash(RowDetail, RowHash),
  constraint_row(Row, RowHash),
  sumlist(RowDetail, RowSum),
  sum(Row, #=, RowSum).

constraint_row(Row, RowHash) :-
  constraint_row(Row, 1, 0, 0, RowHash).

constraint_row([], _, _, Acc, Row) :- Acc #= Row.
constraint_row([H|T], PowerOfTwo, PrevH, Acc, Row) :-
    PowerOfTwo1 #= PowerOfTwo + PowerOfTwo * max(H, PrevH)
  , Acc1 #= Acc + PowerOfTwo*H
  , constraint_row(T, PowerOfTwo1, H, Acc1, Row).

row_hash(RowDetail, RowHash) :-
  row_hash(RowDetail, 1, 0, RowHash).

row_hash([], _, Acc, Acc) :- !.
row_hash([H|T], PowerOfTwo, Acc, Row) :-
  increment(H, PowerOfTwo, Acc, PowerOfTwo1, Acc1),
  PowerOfTwo0 is PowerOfTwo1*2,
  row_hash(T, PowerOfTwo0, Acc1, Row).

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
