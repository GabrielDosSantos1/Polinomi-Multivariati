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
as_variable(X^0,[]) :- atom(X).
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

%% l'ordinamento di sort non funziona come dovrebbe in realta non funziona solo per l'ordinamento lessiografi
ordinamento_polinomi([v(_, _)],[], 1).
ordinamento_polinomi([v(_, X)],[v(_, Y)], 1) :-
	char_code(X,C1),
	char_code(Y,C2),
	C1 > C2.

ordinamento_polinomi([],[],0).
ordinamento_polinomi([v(_, X)],[v(_, Y)], 0) :- %%% questo effetivamento è il coso in cui i due monomi sono uguali
	char_code(X,C1),
	char_code(Y,C2),
	C1 = C2.

ordinamento_polinomi([],[v(_, _)], -1).
ordinamento_polinomi([v(_, X)],[v(_, Y)], -1) :-
	char_code(X,C1),
	char_code(Y,C2),
	C1 < C2.

ordinamento_polinomi([X|RestX],[Y|RestY], Ris) :-
	ordinamento_polinomi([X],[Y],Ris),
	ris \= 0.

ordinamento_polinomi([X|RestX],[Y|RestY], Ris) :-
	ordinamento_polinomi([X],[Y],0),
	ordinamento_polinomi(RestX,RestY,Ris).

spareggio([],[]).
spareggio([m(_,_,M1),m(_,_,M2)|Resto],[m(_,_,M2)|RestOrd]):-
	ordinamento_polinomi(M1,M2,1),
	spareggio(Resto, RestOrd).

spareggio([],[]).
spareggio([m(C1,T1,M1)],[m(C1,T1,M1)]).

spareggio([m(C1,T1,M1),m(C2,T2,M2)|Resto],[m(C1,T1,M1)|RestOrd]):-
	ordinamento_polinomi(M1,M2,1),
	append([m(C2,T2,M2)],Resto,Ric),
	spareggio(Ric, RestOrd).

spareggio([m(C1,T1,M1),m(C2,T2,M2)|Resto],[m(C2,T2,M2)|RestOrd]):-
	ordinamento_polinomi(M1,M2,-1),
	append([m(C1,T1,M1)],Resto,Ric),
	spareggio(Ric, RestOrd).

spareggio([m(C1,T1,M1),m(C2,T2,M2)|Resto],[m(C2,T2,M2)|RestOrd]):- %%% TODO : FINIRE MANCA IL CASO IN CUI SONO UGUALI
	ordinamento_polinomi(M1,M2,0),
	append([m(C1,T1,M1)],Resto,Ric),
	spareggio(Ric, RestOrd).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
as_polynomial(Expression,poly(Sorted1)):-
	monomi(Expression,Monomi), %% devo controllare se ho dei duplicati
	spareggio(Monomio, Sorted), %% qui ordino in base alle Variabili
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% coefficients
coefficients(Poly(Monomi), Coefficients) :-
	find_coefficients(Monomi,Coefficients).

find_coefficients([m(C ,_ ,_)|Resto],[C|R]):-
	find_coefficients(Resto,R).

find_coefficients([],[]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% variables
% TODO: bisogna gestire il caso in cui ci sono simboli duplicati.
variables(poly(Monomi), Variables):-
	find_monomi(Monomi,Variables).

find_monomi([],[]).
find_monomi([m(_, _, Var)| Resto], Soluzione):-
	get_variables(Var,Variables),
	find_monomi(Resto, Ricorsione),
	append(Variables, Ricorsione, Soluzione).

get_variables([],[]).
get_variables([v(_,Symbol)|Resto],[Symbol|Ricorsione]):-
	get_variables(Resto,Ricorsione).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%monomials(Poly, Monomials)
%TODO: risolvere il problema dell'ordinamento
monomials(poly(Monomials), Monomials):-
	spareggio(Monomio, Sorted), %% qui ordino in base alle Variabili
	sort(2, @>= , Sorted, Monomials). %% qui ordino secondo il grado dei monomi.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% maxdegree(Poly, Degree)
%TODO: trovare il massimo di una lista
maxdegree(poly(Monomi), Soluzione):-
	find_degree(Monomi, Degree).

find_degree([],[]).
find_degree([m(_, _, Var)| Resto], Soluzione):-
	get_degree(Var,Variables),
	find_degree(Resto, Ricorsione),
	append(Variables, Ricorsione, Soluzione).

get_degree([v(_,Symbol)|Resto],[Symbol|Ricorsione]):-
	get_degree(Resto,Ricorsione).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% end of file -- polinomimultivariati.pl --
