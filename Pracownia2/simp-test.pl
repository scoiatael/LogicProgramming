% vi: syntax=prolog, filetype=prolog
simpTest1(3+3,1).
simpTest2((z*y+(2*x-3*x+(1-2)*y)*z)*(10-11),3).
simpTest3(x+y+(x-(z+x))+2*z,5).
simpTest4(2*x*3*y*4,5).
simpTest5(x*(1-x+(2+x))+(4-(x+y*z)+x-1)+z*y,5).
simpTest6(x+y*z+x*y-(z*(2*x-x)+1)+5-(x+2*x+y+x)+y-(z+x)*y,9).

termSize(T,1) :- ( var(T); number(T) ), !.
termSize(T,1) :- atom(T),!.
termSize(T,S) :-
  T =.. [_|Args], argSize(Args,1,S).
argSize([],S,S).
argSize([X|Xs], A, S) :-
  termSize(X,S1),
  A1 is A+S1,
  argSize(Xs,A1,S).
