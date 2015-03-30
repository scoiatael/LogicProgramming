% zadanie 4: reverse przy uzyciu append
reverse([], []).
reverse([H|T], Y) :- reverse(T,Tp), append(Tp, [H], Y).

% zadanie 5
% bledy w predykacie prefix
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
writeLisp(A) :- atomic(A), write(A).
writeLisp(F) :- F =.. [H|T], T=[_|_], writef("("), write(H), writeLispList(T), writef(")").
writeLispList([]).
writeLispList([H|T]) :- writef(" "), writeLisp(H), writeLispList(T).

% 7 shuffle_term
% w przykladzie jest blad: tam sa dowolne permutacje dzieci?
shuffle_term(X,Y):- X=..[H|T], shuffle_term_list(T, Tp), Y=..[H|Tp].
shuffle_term_list(As, Bs) :- append(I, [X|T], As), append(Ip, [Y|Tp], T), append(Ip, [X|Tp], Tpp), append(I, [Y|Tpp], Bs).
shuffle_term_list(A, B) :- append(Hs,[H|T],A), shuffle_term(H,Hp), append(Hs,[Hp|T],B).
