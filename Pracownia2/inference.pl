% vi: syntax=prolog, filetype=prolog

inference(Facts, Rules, MaxLength, Result) :-
  map_addFact(Facts),
  infereTillFull(Facts, Rules, MaxLength, Result),
  dropFacts.

infereTillFull(Facts, Rules, MaxLength, Result) :-
  member(Rule, Rules),
  infere(Rule, SomeResults),
  has_newFacts(SomeResults),
  drop_longFacts(SomeResults, MaxLength, ShortFacts),
  filter_uniqueFacts(Facts, ShortFacts, NewFacts),
  \+ NewFacts = [], !,
  map_addFact(NewFacts),
  append(NewFacts, Facts, AllFacts),
  infereTillFull(AllFacts, Rules, MaxLength, Result).
infereTillFull(Facts, _, _, Facts).

infere(X>>R, AllResults) :-
  copy_term(X>>R, Premises>>Result),
  findall(Result, check(Premises), Results),
  flatten(Results, AllResults).

check([]).
check([H|T]) :- fact(H), check(T).

has_newFacts([H|_]) :- \+ fact(H), !.
has_newFacts([_|T]) :- has_newFacts(T).

drop_longFacts([], _, []).
drop_longFacts([H|T], MaxLength, [H|Tp]) :- term_size(H, S), S =< MaxLength, !, drop_longFacts(T, MaxLength, Tp).
drop_longFacts([_|T], MaxLength, Tp) :- drop_longFacts(T, MaxLength, Tp).

filter_uniqueFacts(_, [], []) :- !.
filter_uniqueFacts(Facts, [H|T], [H|Tp]) :- \+ member(H, Facts), !, filter_uniqueFacts(Facts, T, Tp).
filter_uniqueFacts(Facts, [_|T], Tp) :- filter_uniqueFacts(Facts, T, Tp).

map_addFact([]).
map_addFact([X|T]) :- asserta(fact(X)), map_addFact(T).

dropFacts :- retractall(fact(_)).

term_size(T,1) :- ( var(T); number(T) ), !.
term_size(T,1) :- atom(T),!.
term_size(T,S) :-
  T =.. [_|Args], arg_size(Args,1,S).
arg_size([],S,S).
arg_size([X|Xs], A, S) :-
  term_size(X,S1),
  A1 is A+S1,
  arg_size(Xs,A1,S).
