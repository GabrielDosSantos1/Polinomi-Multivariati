%%%% -*- Mode: Prolog -*-
%%%% polinomimultivariati.pl --

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% coefficients
coefficients(poly(Monomi), Coefficients) :-
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
	find_variables(Var,Variables),
	find_monomi(Resto, Ricorsione),
	append(Variables, Ricorsione, Soluzione).

find_variables([],[]).
find_variables([v(_,Symbol)|Resto],[Symbol|Ricorsione]):-
	find_variables(Resto,Ricorsione).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%monomials(Poly, Monomials)
%TODO: risolvere il problema dell'ordinamento
monomials(poly(Monomials), Monomials).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% maxdegree(Poly, Degree)
maxdegree(poly(Monomi), Soluzione):-
	find_degree(Monomi, Degrees),
	max_list(Degrees, Soluzione)  .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%mindegree(Poly, Degree).
mindegree(poly(Monomi), Soluzione):-
	find_degree(Monomi, Degrees),
	min_list(Degrees, Soluzione).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_degree([],[]).
find_degree([m(_, Degree, _)| Resto], [Degree|Ric]):-
	find_degree(Resto, Ric).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%polyplus(Poly1, Poly2, Result)
%TODO: fare l'ordinamento
polyplus(Poly1, Poly2, poly(Soluzione)) :-
	Poly1 = poly(Monomi1),
	Poly2 = poly(Monomi2),
	append(Monomi1, Monomi2, Monomi),
	ordina_monomi(Monomi, Soluzione).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%polyminus
polyminus(Poly1, Poly2, poly(Soluzione)):-
	polytimes(poly(m(-1,0,[])), Poly2, Poly2PerMinusOne),
	polyplus(Poly1, Poly2PerMinusOne, poly(Soluzione)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%polytimes(Poly1, Poly2, Result)
%TODO: ritorna un polinomio bisogna ordinarlo.
polytimes(poly([]), _ ,poly([])).

polytimes(poly([M|Resto]), poly(Monomi2), poly(Soluzione)) :-
	scalare(M, Monomi2, Primo),
	polytimes(poly(Resto) ,poly(Monomi2), poly(SoluzioneRic)),
	append(Primo ,SoluzioneRic, Soluzione).

scalare(m(_, _, _), [] , []).
scalare(m(C1, TD1, Var1), [m(C2, TD2, Var2)|Resto], [m(C, TD, Var)|Ric]) :-
	C is C1 * C2,
	TD is TD1 + TD2,
	append(Var1,  Var2, Var),
	scalare(m(C1, TD1, Var1), Resto, Ric).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%polyval(Polynomial, VariableValues, Value)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%pprint_polynomial(Polynomial)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% end of file -- polinomimultivariati.pl --
