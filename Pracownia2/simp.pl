% vi: syntax=prolog, filetype=prolog

simp(St, Y) :- prepare(St, X), findall(Z, simplify(X, Z), B), find_best(B, Best), convert_back(Best, Y).

prepare(X, Y) :- elim_minus(X, Z, 1), push_to_common_root(Z, Y), !.

simplify(X, Z) :-
  push_to_common_root(X, Xp),
  sort_tree(Xp, Xpp),
  cluster(Xpp, Xppp),
  distribute(Xppp, Y),
  loop_simplify(X, Y, Z).

loop_simplify(X, Y, Z) :- \+ X=Y, !, simplify(Y, Z).
loop_simplify(X, X, X) :- !.

elim_minus(X, X, 1) :- atomic(X), !.
elim_minus(X, F, -1) :- atom(X), !, F =.. [(*), -1, X].
elim_minus(X, F, -1) :- number(X), !, F is -X.
elim_minus(F, Fp, V) :- F =.. [(-), A, B], !, elim_minus(A, Ap, V), Vb is V*(-1), elim_minus(B, Bp, Vb), Fp =.. [(+), Ap, Bp].
elim_minus(F, Fp, V) :- F =.. [(+), A, B], elim_minus(A, Ap, V), elim_minus(B, Bp, V), Fp =.. [(+), Ap, Bp].
elim_minus(F, Fp, V) :- F =.. [(*), A, B], elim_minus(A, Ap, V), elim_minus(B, Bp, 1), Fp =.. [(*), Ap, Bp].

push_to_common_root(X, X):- atomic(X), !.
push_to_common_root(Z, F):- Z =.. [Op | Li], map_push_to_common_root(Li, Lip), fold_append_by_op(Op, Lip, F).

map_push_to_common_root([], []).
map_push_to_common_root([H|T], [Hp|Tp]) :- push_to_common_root(H, Hp), map_push_to_common_root(T, Tp).

append_by_op(Op, A, B, F) :- A =.. [Op | Ap], B =.. [Op | Bp], !, append(Ap, Bp, Fp), F =.. [Op | Fp].
append_by_op(Op, A, B, F) :- A =.. [Op | Ap], !, F =.. [Op, B | Ap].
append_by_op(Op, A, B, F) :- B =.. [Op | Bp], !, F =.. [Op, A | Bp].
append_by_op(Op, A, B, F) :- F =.. [Op, A, B].

fold_append_by_op(_, [H], H) :- !.
fold_append_by_op(Op, [H|T], Ff) :- fold_append_by_op(Op, T, Tp), append_by_op(Op, H, Tp, Ff).

sort_tree(X, X) :- atomic(X), !.
sort_tree(F, Fp) :- F =.. [Op | Li], map_sort_tree(Li, Lip), !, msort(Lip, Lipp), Fp =.. [Op | Lipp].

map_sort_tree([], []).
map_sort_tree([X|Y], [Xp|Yp]) :- sort_tree(X, Xp), map_sort_tree(Y, Yp).

cluster(X, X) :- atomic(X), !.
cluster(F, Fp) :-
  F =.. [Op | Li],
  map_cluster(Li, Lip),
  fold_cluster(Op, Lip, Lipp),
  elim_singleton_op(Op, Lipp, Fp), !.

map_cluster([], []).
map_cluster([X|Y], [Xp|Yp]) :- cluster(X, Xp), map_cluster(Y, Yp).

fold_cluster((-), X, F) :- cluster_num((-), X, Xp, Y), F = [ Xp | Y].
fold_cluster((*), X, F) :- cluster_num((*), X, Xp, Y), crunch_factors(1, Xp, Y, F).
fold_cluster((+), X, F) :- cluster_num((+), X, Xp, Y), cluster_vars(Y, Yp), crunch_factors(0, Xp, Yp, F).

crunch_factors(ZValue, ZValue, [], [ZValue]) :- !.
crunch_factors(ZValue, ZValue, Vars, Vars) :- !.
crunch_factors(_, Num, Vars, [Num | Vars]) :- !.

cluster_num(Op, X, Y, Z) :- take_numbers(X, Xp, Z), eval_list(Op, Xp, Y).

eval_list((+), [], 0).
eval_list((*), [], 1).
eval_list((-), [H|T], S) :- eval_list((-), H, T, S).
eval_list(Op, [H|T], S) :- eval_list(Op, T, Sp), Ev =.. [Op, Sp, H], S is Ev.
eval_list((-), H, [], H).
eval_list((-), X, [H|T], S) :- Sp is X - H, eval_list((-), Sp, T, S).


take_numbers([X|Y], [X|Yp], Z) :- number(X), !, take_numbers(Y, Yp, Z).
take_numbers(Y, [], Y).

cluster_vars([], []).
cluster_vars([X|Y], [Xp|Yp]) :- count(X, [X|Y], N, R), cluster_vars(R, Yp), (N = 1, !, Xp = X; Xp =.. [(*), X, N]).

count(X, [X|Y], N, R) :- !, count(X, Y, Np, R), N is Np + 1.
count(_, Y, 0, Y).

distribute(X, X) :- atomic(X), !.
distribute(F, Fp) :-
  F =.. [Op | Li],
  map_distribute(Li, Lip),
  (member(Op, [(+)]),
    group_by_common_factor(Lip, Lipp),
    \+ Lip = Lipp,
    elim_singleton_op(Op, Lipp,  Fp)
  ;
    Fp =.. [Op | Lip]).

elim_singleton_op(_, [Fpp], Fpp):- !.
elim_singleton_op(Op, Lipp, Fpp) :- Fpp =.. [Op | Lipp].

map_distribute([], []).
map_distribute([H|T], [Hp|Tp]) :- distribute(H, Hp), map_distribute(T, Tp).

group_by_common_factor([], []).
group_by_common_factor([H|T], [Lpp | D]) :-
  select_arg(H, A, R),
  find_common(A, T, C, D),
  \+ C = [],
  Fac =.. [(+), R | C],
  Mult =.. [(*), A, Fac],
  elim_singleton_op((+), [Mult | D], Lpp).
group_by_common_factor([H|T], [H|D]) :- group_by_common_factor(T, D).

select_arg(H, Arg, Rest) :-
  H=.. [Op | Args],
  \+ Args = [],
  Op = (*),
  select(Arg, Args, R),
  elim_singleton(R, Rest).
select_arg(Arg, Arg, 1).

elim_singleton([R], R) :- !.
elim_singleton(R, R).

find_common(_, T, [], T).
find_common(A, [H|T], [Rp|C], D) :- H =.. [(*) | Args], select(A, Args, R), elim_singleton_op((*), R, Rp), find_common(A, T, C, D).

find_best(Opts, Best) :- map_term_size(Opts, Optsp), find_shortest(Optsp, _-Best).

term_size(X, 1) :- atomic(X),!.
term_size(F, T) :- F =.. [_ | Li], reduce_term_size(Li, S), sum_list(S, T).
reduce_term_size([], []).
reduce_term_size([H|T], [S | Ts]) :- term_size(H, S), reduce_term_size(T, Ts).

map_term_size([], []).
map_term_size([H|T], [S-H|Tp]) :- term_size(H, S), map_term_size(T, Tp).

find_shortest(T, Z) :- keysort(T, [Z | _]).

convert_back(M, M) :- atomic(M), !.
convert_back(M, S) :- M =.. [Op | Li], map_convert_back(Li, Lip), group_with(Op, Lip, S), !.
map_convert_back([], []).
map_convert_back([H|T], [Hp|Tp]) :- convert_back(H, Hp), map_convert_back(T, Tp).
group_with(_, [A], A).
group_with(Op, [H|T], F) :- group_with(Op, T, Fp), F =.. [Op, H, Fp].
