:- use_module(library(clpfd)), dynamic(hash/3).

generate_possibilities(_, []) :- !.
generate_possibilities(N, [(H,Hash)|T]) :-
  length(Li, N),
  findall(_, (generate_row(Li, Li, H), \+ hash(N, Hash, Li), asserta(hash(N, Hash, Li))), _),
  generate_possibilities(N, T).

generate_row(Li, _, []) :- all_one_or_zero(Li).
generate_row(Li, Working, [H|T]) :-
  ones_and_zero(H, LiBeg, T),
  jump_some(Working, LiNew),
  append(LiBeg, LiEnd, LiNew),
  generate_row(Li, LiEnd, T).
ones_and_zero(X, Li, T) :- length(L, X), maplist(const_one, L, L1), (T=[], L1 = Li; append(L1, [0], Li)).
const_one(_, 1).

all_one_or_zero([]).
all_one_or_zero([X|T]) :- nonvar(X), !, all_one_or_zero(T).
all_one_or_zero([0|T]) :- all_one_or_zero(T).

jump_some([], []).
jump_some([H|T], [H|T]).
jump_some([_|T], T0) :- jump_some(T, T0).

nono(Rzedy, Kolumny, B) :-
  length(Rzedy, NRzedow),
  length(Kolumny, NKolumn),
  maplist(row_hash, Rzedy, RzedyHashed),
  maplist(row_hash, Kolumny, KolumnyHashed),
  zip(Rzedy, RzedyHashed, RzedyRzedyHashed),
  zip(Kolumny, KolumnyHashed, KolumnyKolumnyHashed),
  generate_possibilities(NKolumn, RzedyRzedyHashed),
  generate_possibilities(NRzedow, KolumnyKolumnyHashed),
  length(B, NRzedow),
  maplist(length_swapped(NKolumn), B),
  flatten(B, Vars),
  Vars ins 0 \/ 1,
  transpose(B, BTransp),
  constraint_rows_by_possibilities(B, RzedyHashed, NKolumn),
  constraint_rows_by_possibilities(BTransp, KolumnyHashed, NRzedow),
  labeling([ffc, enum], Vars).

constraint_rows_by_possibilities(B, Rows, NCol) :-
  zip(B, Rows, Zipped),
  maplist(constraint_rows_by_possibilities_aux(NCol), Zipped).

constraint_rows_by_possibilities_aux(N, (Row, RowHash)) :-
  findall(Li, hash(N, RowHash, Li), Poss),
  tuples_in([Row], Poss).

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
