% vi: syntax=prolog, filetype=prolog

inference(Facts, Rules, MaxLength, Result) :-
  map_addFact(Facts, []),
  length(Facts, Length),
  infereTillFull(Facts, Rules, MaxLength, Length, Result),
  dropFacts.

infereTillFull(Facts, _, MaxLength, CurLength, Facts) :- CurLength >= MaxLength-1, !.
infereTillFull(Facts, Rules, MaxLength, CurLength, Result) :-
  member(Rule, Rules),
  infere(Rule, SomeResults),
  ensureUniqueness(SomeResults, Facts, NewFacts),
  length(NewFacts, UpdatedLength),
  UpdatedLength > CurLength, !,
  map_addFact(SomeResults, Facts),
  infereTillFull(NewFacts, Rules, MaxLength, UpdatedLength, Result).
infereTillFull(Facts, _, _, _, Facts).

ensureUniqueness(SomeResults, Facts, NewFacts) :-
  append(Facts, SomeResults, UpdatedFacts),
  sort(UpdatedFacts, NewFacts).

infere(X>>R, Result) :- copy_term(X>>R, Premises>>Result), check(Premises).

check([]).
check([H|T]) :- fact(H), check(T).

map_addFact([], _).
map_addFact([X|T], OldFacts) :- (member(X, OldFacts), !; asserta(fact(X))), map_addFact(T, OldFacts).

dropFacts :- retractall(fact(_)).
