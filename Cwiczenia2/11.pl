assertp(X) :- assertz(X).
assertn(X) :- BangFail =.. [',', !, fail], F =.. [':-', X, BangFail], asserta(F).
