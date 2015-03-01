% zadanie 4: reverse przy uzyciu append
reverse([], []).
reverse([H|T], Y) :- reverse(T,Tp), append(Tp, [H], Y).
