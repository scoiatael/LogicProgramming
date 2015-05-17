:- dynamic(was_here/1).

solve(Start, End, Solution) :-
  retractall(was_here(_)),
  f_value(Start, 0, End, F),
  search([(F, (Start,0,[]))], End, S),
  reverse(S, Solution).

f_value(X, Depth, E, Value) :-
  h_value(X, E, H), !,
  Value is H+Depth.

search([(_, (End,_,Sol)) | _], End, Sol).
search([(_, State)|R], End, Solution) :-
  State = (S, _, _),
  ( fail, was_here(S), !, Next = R
  ; write_state(State),
    expand(State, End, States),
    assert(was_here(S)),
    append(States, R, Rest),
    sort(Rest, Next)
  ),
  search(Next, End, Solution).

write_state((S, _, _)) :- pretty_print(S), nl.
pretty_print([A1, A2, A3, A4, A5, A6, A7, A8, A9]) :-
  write([A1, A2, A3]), nl,
  write([A4, A5, A6]), nl,
  write([A7, A8, A9]), nl.

expand((State,D,S), E, Nexts) :-
  D1 is D+1,
  bagof((F, (C,D1,[Move|S])),
  ( move(State, C, Move), % \+ was_here(C),
    f_value(C, D1, E, F)),
    Nexts
  ).

osemka(Start, End, Num) :- solve(Start, End, S), !, length(S, Num).

h_value(_, _, 0).
%h_value(Li, Final, N) :-
%  %  Li = [A1,A2,A3,A4,A5,A6,A7,A8,A9],
%  manhattan(Li, Final, M),
%  out_of_order(Li, Final, O),
%  N is M+3*O.

number_vars([], _, []).
number_vars([H|T], N, [(H,N)|T0]) :- N0 is N+1, number_vars(T, N0, T0).
sumlist([], Acc, Acc).
sumlist([H|T], Acc, Sol) :- Acc0 is Acc+H, sumlist(T, Acc0, Sol).

manhattan(Li, [F|Fi], M) :-
  Final = [F|Fi],
  number_vars(Li, 0, LiN),
  maplist(manhattan(Final), LiN, Ns),
  sumlist(Ns, 0, M).

manhattan(E, (W, P), V) :- append(Beg, [W|_], E), length(Beg, M), V is abs(M-P).

out_of_order(Li, F, O) :-
  out_of_order(Li, O1),
  out_of_order(F, O2),
  O is abs(O1-O2).

map_pairs(_, [_], []).
map_pairs(Go, [H1,H2|T], [P|Tp]) :- call(Go, (H1, H2), P), map_pairs(Go, [H2|T], Tp).

out_of_order(Li, O) :-
  map_pairs(out_of_order, Li, Os),
  sumlist(Os, O).

out_of_order((X, Y), 0) :- member(o, [X,Y]), !.
out_of_order((X, Y), 0) :- X is Y+1, !.
out_of_order((_, _), 1).

move(Before, After, move) :-
  pos(Before, Num), (
    ( Num = 1, !, ( down(Before, After) ; right(Before, After)))
  ; ( Num = 2, !, ( down(Before, After) ; left_or_right(Before, After)))
  ; ( Num = 3, !, ( down(Before, After) ; left(Before, After)))
  ; ( Num = 4, !, ( up_or_down(Before, After) ; right(Before, After)))
  ; ( Num = 5, !, ( up_or_down(Before, After) ; left_or_right(Before, After)))
  ; ( Num = 6, !, ( up_or_down(Before, After) ; left(Before, After)))
  ; ( Num = 7, !, ( up(Before, After) ; right(Before, After)))
  ; ( Num = 8, !, ( up(Before, After) ; left_or_right(Before, After)))
  ; ( Num = 9, !, ( up(Before, After) ; left(Before, After)))
  ).

pos(B, N) :- append(Beg, [o|_], B), length(Beg, N0), N is N0+1.

left(B, A) :- append(Beg, [X, o|E], B), append(Beg, [o, X|E], A).
right(B, A) :- append(Beg, [o, X|E], B), append(Beg, [X, o|E], A).
left_or_right(B, A) :- left(B, A) ; right(B, A).

up(B, A) :- append(Beg, [A1, A2, A3, o | E], B), append(Beg, [o, A1, A2, A3 | E], A).
down(B, A) :- append(Beg, [o, A1, A2, A3 | E], B), append(Beg, [A1, A2, A3, o | E], A).
up_or_down(B, A) :- up(B, A) ; down(B, A).
