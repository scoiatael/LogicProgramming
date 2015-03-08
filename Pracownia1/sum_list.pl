% vi: syntax=prolog, filetype=prolog
sum_list(L, S) :- sum_list_aux(L, 0, S).

sum_list_aux([], S, S).
sum_list_aux([H|T], SP, S) :- SPp is SP + H, sum_list_aux(T, SPp, S).
