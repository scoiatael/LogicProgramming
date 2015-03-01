% zadanie 4: reverse przy uzyciu append
reverse([], []).
reverse([H|T], Y) :- reverse(T,Tp), append(Tp, [H], Y).

% zadanie 5
% dane:
prefix([],_).
prefix([H|Xs],[H|Ys]) :- prefix(Xs,Ys).

suffix(Xs,Xs).
suffix(Xs,[_|Ys]) :- suffix(Xs,Ys).

% a) X to ciagly fragment Y
sublist(X, Y) :- prefix(Pr, Y), suffix(X, Pr), X = [_|_].
sublist([], _).

% b) member za pomocą sublist
member_sublist(X, Y) :- sublist([X], Y).

% c) member za pomocą append
member_append(X,Y) :- append(_, [X |_], Y).

% d) prefix i suffix za pomocą append
prefix_append(X,Y) :- append(X, _, Y).
suffix_append(X,Y) :- append(_, X, Y).

% e) adjacent za pomocą append
adjacent_append(X, Y, L) :- append(_, [X,Y | _], L).

% f) last za pomocą append
last_append(X,L) :- append(_, [X], L).

% 6 writeLisp
writeLisp(F) :- flattenLisp(F, Fp), write(Fp).
flattenLisp(A, A) :- atomic(A).
flattenLisp(F, (H,Tp)) :- F =.. [H|T], T=[_|_], flattenLispList(T, Tp).
flattenLispList([],[]).
flattenLispList([H|T], [Hp|Tp]) :- flattenLisp(H, Hp), flattenLispList(T,Tp).
