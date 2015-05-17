:- use_module(library(clpfd)).

aryt(A+B=C, Sol) :-
  atom_string(A, AStr), atom_string(B, BStr), atom_string(C, CStr),
  string_codes(AStr, ACodes),
  string_codes(BStr, BCodes),
  string_codes(CStr, CCodes),
  append(BCodes, CCodes, StrP), append(ACodes, StrP, Str),
  codesToBinding(Str, [], Binding), !,
  varsFromBinding(Binding, Vars),
  Vars ins 0..9,
  all_different(Vars),
  words(ACodes, Binding, [], First),
  words(BCodes, Binding, [], Second),
  words(CCodes, Binding, [], Third),
  nonZeroFirstLetters(First, Second, Third),
  vars_to_number(First, 0, FirstNumber), vars_to_number(Second, 0, SecondNumber), vars_to_number(Third, 0, ThirdNumber),
  FirstNumber + SecondNumber #= ThirdNumber,
  labeling([ff, bisect], Vars),
  solFromBindings(Binding, Sol).

codesToBinding([], B, B).
codesToBinding([H|T], Bind, Binding) :- member((H, _), Bind), codesToBinding(T, Bind, Binding).
codesToBinding([H|T], Bind, Binding) :- string_codes(Str, [H]),atom_string(At, Str), codesToBinding(T, [(H, _, At)|Bind], Binding).

varsFromBinding([], []).
varsFromBinding([(_, L, _)|T], [L|Tp]) :- varsFromBinding(T, Tp).

nonZeroFirstLetters([F|_], [S|_], [T|_]) :- F #\= 0, S #\= 0, T #\= 0.

vars_to_number([], Acc, Num) :- Acc #= Num.
vars_to_number([H|T], Acc, Num) :- vars_to_number(T, H+10*Acc, Num).

words([], _, Acc, Val) :- reverse(Acc, Val), !.
words([H|T], Binding, Acc, Val) :- member((H, V, _), Binding), !, words(T, Binding, [V|Acc], Val).

solFromBindings([], []).
solFromBindings([(_, S, L)|T], [(L, S)|Sol]) :- solFromBindings(T, Sol).
