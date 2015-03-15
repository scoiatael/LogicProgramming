% vi: syntax=prolog, filetype=prolog

simplification(X, Y) :- findall(Z, simplify(X, Z), B), find_best(B, Y).

simplify(X, Z) :- push_to_common_root(X, Xp), sort_tree(Xp, Xpp), cluster(Xpp, Xppp), distribute(Xppp, Y), ( \+ X = Y, simplify(Y, Z); Y = Z).

push_to_common_root(X, X):- atomic(X), !.
push_to_common_root(Z, F):- Z =.. [Op, A, B], \+ Op = (/), push_to_common_root(A, Ap), push_to_common_root(B, Bp), append_by_op(Op, Ap, Bp, F).

append_by_op(Op, A, B, F) :- A =.. [Op | Ap], B =.. [Op | Bp], !, append(Ap, Bp, Fp), F =.. [Op | Fp].
append_by_op(Op, A, B, F) :- A =.. [Op | Ap], !, F =.. [Op, B | Ap].
append_by_op(Op, A, B, F) :- B =.. [Op | Bp], !, F =.. [Op, A | Bp].
append_by_op(Op, A, B, F) :- F =.. [Op, A, B].

sort_tree(X, X) :- atomic(X), !.
sort_tree(F, Fp) :- F =.. [Op | Li], map_sort_tree(Li, Lip), !, msort(Lip, Lipp), Fp =.. [Op | Lipp].

map_sort_tree([], []).
map_sort_tree([X|Y], [Xp|Yp]) :- sort_tree(X, Xp), map_sort_tree(Y, Yp).

cluster(X, X) :- atomic(X), !.
cluster(F, Fp) :- F =.. [Op | Li], map_cluster(Li, Lip), fold_cluster(Op, Lip, Lipp), elim_singleton_op(Op, Lipp, Fp).

map_cluster([], []).
map_cluster([X|Y], [Xp|Yp]) :- cluster(X, Xp), map_cluster(Y, Yp).

fold_cluster((*), X, F) :- cluster_num((*), X, Xp, Y), (Xp = 1, !, F = Y; F = [ Xp | Y]).
fold_cluster((+), X, F) :- cluster_num((+), X, Xp, Y), cluster_vars(Y, Yp), (Xp = 0, !, F = Yp; F = [Xp | Yp]).
cluster_num(Op, X, Y, Z) :- take_numbers(X, Xp, Z), eval_list(Op, Xp, Y).
eval_list((+), [], 0).
eval_list((*), [], 1).
eval_list(Op, [H|T], S) :- eval_list(Op, T, Sp), Ev =.. [Op, Sp, H], S is Ev.

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
  (Op = (+),
    group_by_common_factor(Lip, Lipp),
    \+ Lip = Lipp,
    elim_singleton_op(Op, Lipp,  Fpp),
    distribute(Fpp, Fp)
  ;
    Fp =.. [Op | Lip]).

elim_singleton_op(_, [Fpp], Fpp):- !.
elim_singleton_op(Op, Lipp, Fpp) :- Fpp =.. [Op | Lipp].

map_distribute([], []).
map_distribute([H|T], [Hp|Tp]) :- distribute(H, Hp), map_distribute(T, Tp).

group_by_common_factor([], []).
group_by_common_factor([H|T], [Lpp | D]) :-
  H=.. [Op | Args],
  Op = (*),
  select(A, Args, R),
  elim_singleton(R, Rp),
  find_common(A, T, C, D),
  \+ C = [],
  Fac =.. [(+), Rp | C],
  Mult =.. [(*), A, Fac],
  elim_singleton_op((+), [Mult | D], Lpp).
group_by_common_factor([H|T], [H | D]) :- group_by_common_factor(T, D).

elim_singleton([R], R) :- !.
elim_singleton(R, R).

find_common(_, T, [], T).
find_common(A, [H|T], [Rp|C], D) :- H =.. [(*) | Args], select(A, Args, R), elim_singleton(R, Rp), find_common(A, T, C, D).

find_best(Opts, Best) :- map_term_size(Opts, Optsp), find_shortest(Optsp, _-Best).

term_size(X, 1) :- atomic(X),!.
term_size(F, T) :- F =.. [_ | Li], reduce_term_size(Li, S), sum_list(S, T).
reduce_term_size([], []).
reduce_term_size([H|T], [S | Ts]) :- term_size(H, S), reduce_term_size(T, Ts).

map_term_size([], []).
map_term_size([H|T], [S-H|Tp]) :- term_size(H, S), map_term_size(T, Tp).

find_shortest(T, Z) :- keysort(T, [Z | _]).
