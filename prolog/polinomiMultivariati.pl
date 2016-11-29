%%%% -*- Mode: Prolog -*-
%%%% polinomimultivariati.pl --

%TODO : NON FUNZIONA as_polynomial(y^4*z*x^5-y*z*r+y^4*r*z^5,P).
% restituisce :
%ERROR: Syntax error: Operator expected
%ERROR: as_polynomial(y^4*z*x^
%ERROR: ** here **
%ERROR: 5−y*z*r+y^4*r*z^5,P) .

is_monomial(m(_C, TD, VPs)) :-
	integer(TD),
	TD >= 0,
	is_list(VPs).

is_varpower(v(Power, VarSymbol)):-
	integer(Power),
	Power >= 0,
	atom(VarSymbol).

is_polynomial(poly(Monomials)):-
	is_list(Monomials),
	foreach(member(M,  Monomials), is_monomial(M)).

as_monomial(Expression, m(Coe, Tot, Var)) :-
	as_variable(Expression, Variable),
	formalizzazione(Variable, Coe, Tot, Var),!,
	is_monomial(m(Coe, Tot, Var)).

as_variable(X*Y, L) :-
	as_variable(X, R),
	!,
	as_variable(Y, E),
	append(R,E,L).

as_variable(C, [C]) :- C\= [], integer(C).
as_variable(-C , [R]) :- C\= [] , integer(C), R is C * -1.
as_variable(X, [v(1,X)]):- X \= [], atom(X), is_varpower(v(1,X)).
as_variable(X^Y, [v(Y,X)]):- !,
	Y >= 0,
	integer(Y),
	atom(X),
	is_varpower(v(Y,X)).

formalizzazione([Coe|Resto], Coe, Tot, Variabili) :-
	integer(Coe),
	merge_sort(Resto,Variabili),
	sumdegree(Variabili,Tot).

formalizzazione(Var, 1, Tot, Variabili) :-
	merge_sort(Var,Variabili),
	sumdegree(Variabili,Tot).

formalizzazione([Coe], Coe, 0, []) :- integer(Coe).

sumdegree([v(N,_)|Resto], R) :-
	sumdegree(Resto,M),!,
	R is N+M.
sumdegree([v(N,_)],N).

merge_sort([v(N,X)],[v(N,X)]).
merge_sort(Lista,Sorted):-
	Lista = [_,_|_],
	divide(Lista,L1,L2),
	merge_sort(L1,Sorted1),
	merge_sort(L2,Sorted2),
	merge(Sorted1,Sorted2,Sorted).

merge([],L,L).
merge(L,[],L) :- L \= [].

merge([v(N,X)|RestX],[v(M,Y)|RestY],[v(N,X)|Rest]) :-
	char_code(X,C1),
	char_code(Y,C2),
	C1 < C2,
	merge(RestX,[v(M,Y)|RestY],Rest).

merge([v(N,X)|RestX],[v(M,Y)|RestY],[v(G,X)|Rest]):-
	char_code(X,C1),
	char_code(Y,C2),
	C1 = C2,
	G is N+M,
	merge(RestX,RestY,Rest).

merge([v(N,X)|RestX],[v(M,Y)|RestY],[v(M,Y)|Rest]) :-
	char_code(X,C1),
	char_code(Y,C2),
	C1 > C2,
	merge([v(N,X)|RestX],RestY,Rest).

divide(L,A,B) :- d(L,L,A,B).
d([],R,[],R).
d([_],R,[],R).
d([_,_|T],[X|L],[X|L1],R) :- d(T,L,L1,R).

as_polynomial(Expression,poly(Sorted1)):-
	monomi(Expression,Monomi), %% devo controllare se ho dei duplicati
	sort([3,2], @=< , Monomi, Sorted), %% qui ordino in base alle Variabili
	sort(2, @>= , Sorted, Sorted1). %% qui ordino secondo il grado dei monomi

%TODO cercare bene come vanno ordinati i polinomi

monomi(Ex2+Ex1, Monomi) :-
	!,as_monomial(Ex1, Mo1),
	monomi(Ex2, Mo2),
	append(Mo2, [Mo1], Monomi).

monomi(Ex2-Ex1, Monomi) :-
	!,as_monomial(Ex1, Mo1),
	Mo1 = m(C,P,L),
	Cneg is C * -1,
	monomi(Ex2, Mo2),
	append(Mo2, [m(Cneg,P,L)], Monomi).

monomi(Exp,[Monomio]) :- as_monomial(Exp,Monomio).



%%%% end of file -- polinomimultivariati.pl --
