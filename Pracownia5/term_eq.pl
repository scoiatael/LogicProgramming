solveTermConstraints(List) :- sort(List, Sorted), maplist(call, Sorted).
