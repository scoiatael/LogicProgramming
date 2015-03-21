% vi: syntax=prolog, filetype=prolog

simp(St, Y) :- prepare(St, X), findall(Z, simplify(X, Z), B), find_best(B, Best), convert_back(Best, Y).

prepare(X, Y) :- elim_minus(X, Z, 1), push_to_common_root(Z, Y), !.

pipe([], X, X).
pipe([H|T], Acc, X) :- F =.. [ H, Acc, P], F, pipe(T, P, X).

map(_, [], []) :- !.
map(F, [H|T], [Hp | Tp]) :- Fun =.. [F, H, Hp], Fun, map(F, T, Tp).

simplify(X, Z) :-
  pipe([
    push_to_common_root,
    sort_tree,
    cluster,
    elim_zeroes,
    distribute
  ], X, Y),
  loop_simplify(X, Y, Z).

loop_simplify(X, Y, Z) :- \+ X=Y, !, simplify(Y, Z).
loop_simplify(X, X, X) :- !.

elim_zeroes(F, F) :- atomic(F), !.
elim_zeroes(F, Fp) :- F =.. [Op | Args], map(elim_zeroes, Args, X),
  ( Op = (*), (member(0, X), !, Xp = [0] ; Xp = X)
  ; Op = (+), filter_zeroes(X, Xp)
  ), elim_singleton_op(Op, Xp, Fp), !.

filter_zeroes([], []) :- !.
filter_zeroes([0|T], Tp) :- !, filter_zeroes(T, Tp).
filter_zeroes([H|T], [H|Tp]) :- filter_zeroes(T, Tp).

elim_minus(X, X, 1) :- atomic(X), !.
elim_minus(X, F, -1) :- atom(X), !, F =.. [(*), -1, X].
elim_minus(X, F, -1) :- number(X), !, F is -X.
elim_minus(F, Fp, V) :- F =.. [(-), A, B], !, elim_minus(A, Ap, V), Vb is V*(-1), elim_minus(B, Bp, Vb), Fp =.. [(+), Ap, Bp].
elim_minus(F, Fp, V) :- F =.. [(+), A, B], elim_minus(A, Ap, V), elim_minus(B, Bp, V), Fp =.. [(+), Ap, Bp].
elim_minus(F, Fp, V) :- F =.. [(*), A, B], elim_minus(A, Ap, V), elim_minus(B, Bp, 1), Fp =.. [(*), Ap, Bp].

push_to_common_root(X, X):- atomic(X), !.
push_to_common_root(Z, F):- Z =.. [Op | Li], map(push_to_common_root, Li, Lip), fold_append_by_op(Op, Lip, F).

append_by_op(Op, A, B, F) :- A =.. [Op | Ap], B =.. [Op | Bp], !, append(Ap, Bp, Fp), F =.. [Op | Fp].
append_by_op(Op, A, B, F) :- A =.. [Op | Ap], !, F =.. [Op, B | Ap].
append_by_op(Op, A, B, F) :- B =.. [Op | Bp], !, F =.. [Op, A | Bp].
append_by_op(Op, A, B, F) :- F =.. [Op, A, B].

fold_append_by_op(_, [H], H) :- !.
fold_append_by_op(Op, [H|T], Ff) :- fold_append_by_op(Op, T, Tp), append_by_op(Op, H, Tp, Ff).

sort_tree(X, X) :- atomic(X), !.
sort_tree(F, Fp) :- F =.. [Op | Li], map(sort_tree, Li, Lip), !, msort(Lip, Lipp), Fp =.. [Op | Lipp].

cluster(X, X) :- atomic(X), !.
cluster(F, Fp) :-
  F =.. [Op | Li],
  map(cluster, Li, Lip),
  fold_cluster(Op, Lip, Lipp),
  elim_singleton_op(Op, Lipp, Fp), !.

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
  map(distribute, Li, Lip),
  (member(Op, [(+)]),
    group_by_common_factor(Lip, Lipp),
    \+ Lip = Lipp,
    !,
    elim_singleton_op(Op, Lipp,  Fp)
  ;
    Fp =.. [Op | Lip]).

elim_singleton_op(_, [Fpp], Fpp):- !.
elim_singleton_op(Op, Lipp, Fpp) :- Fpp =.. [Op | Lipp].

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

find_best(Opts, Best) :- map(term_size, Opts, Optsp), find_shortest(Optsp, _-Best).

term_size(X, 1-X) :- atomic(X),!.
term_size(F, T-F) :- F =.. [_ | Li], reduce_term_size(Li, S), sum_list([1 | S], T).

reduce_term_size([], []).
reduce_term_size([H|T], [S | Ts]) :- term_size(H, S), reduce_term_size(T, Ts).

find_shortest(T, Z) :- keysort(T, [Z | _]).

convert_back(M, M) :- atomic(M), !.
convert_back(M, S) :- M =.. [Op | Li], map(convert_back, Li, Lip), group_with(Op, Lip, S), !.

group_with(_, [A], A).
group_with(Op, [H|T], F) :- group_with(Op, T, Fp), F =.. [Op, H, Fp].
