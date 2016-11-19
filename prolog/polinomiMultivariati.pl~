%%%% -*- Mode: Prolog -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% polinomimultivariati.pl --

%%%% i monomi devono essere rappresentatiti cosi:

%%m(Coefficient, TotalDegree, VarsPowers).

is_monomial(m(_C, TD, VPs)) :-
	integer(TD),  %si accerta che sia un intero
	TD >= 0,      %e maggiore di 0
	is_list(VPs).


%%%% VarsPowers VPs e una lista ed deve essere rappresentata cosi:

%%v(Power, VarSymbol).

is_varpower(v(Power, VarSymbol)):-
	integer(Power),
	Power >= 0,
	atom(VarSymbol).

%%%% I polinomi devono essere rappresentati cosi:

%%poly(Monomials).

is_polynomial(poly(Monomials)):-
	is_list(Monomials),
	foreach(member(M,  Monomials), is_monomial(M)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   monomi è una lista di polinomi
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%	Il predicato coefficients `e vero quando Coefficients `e una lista dei
%	coefficienti di Poly.

coefficients(Poly, Coefficients) :-
	is_polynomial(poly(Poly)).


%%	Il predicato variables `e vero quando Variables `e una lista dei simboli
%	di variabile che appaiono in Poly.

variables(Poly, Variables).


%%	 Il predicato monomials `e vero quando Monomials `e la lista – ordinata, si
%	 veda sotto – dei monomi che appaiono in Poly.

monomials(Poly ,Monomials).


%%	 Il predicato maxdegree `e vero quando Degree `e il massimo grado dei
%	 monomi che appaiono in Poly.

maxdegree(Poly, Degree).


%%	 Il predicato mindegree `e vero quando Degree `e il minimo grado dei monomi
%	 che appaiono in Poly.

mindegree(Poly, Degree).


%%	 Il predicato polyplus `e vero quando Result `e il polinomio somma di Poly1
%	 e Poly2

polyplus(Poly1, Poly2, Result).


%%	 Il predicato polyminus `e vero quando Result `e il polinomio differenza di
%	 Poly1 e Poly2

polyminus(Poly1, Poly2, Result).


%%	 Il predicato polytimes `e vero quando Result `e il polinomio risultante
%	 dalla moltiplicazione di Poly1 e Poly2.

polytimes(Poly1, Poly2, Result).


%%	 Il predicato as_monomial `e vero quando Monomial `e il termine che
%	 rappresenta il monomio risultante dal “parsing” dell’espressione
%	 Expression; il monomio risultante deve essere appropriatamente ordinato

as_monomial(Expression, Monomial).


%%	Il predicato as polynomial `e vero quando Polynomial `e il termine che
%	 rappresenta il polinomio risultante dal “parsing” dell’espressione
%	 Expression; il polinomio risultante deve essere appropriatamente ordinato

as_polynomial(Expression, Polynomial).


%%	 Il predicato polyval `e vero quanto Value contiene il valore del polinomio
%	 Polynomial (che pu`o anche essere un monomio), nel punto n-dimensionale
%	 rappresentato dalla lista VariableValues, che contiene un valore per ogni
%	 variabile ottenuta con il predicato variables/2.

polyval(Polynomial, VariableValues, Values).


%%	 Il predicato pprint polynomial risulta vedo dopo aver stampato (sullo
%	 “standard output”) una rappresentazione tradizionale del termine polinomio
%	 associato a Polynomial. Si pu´o omettere il simbolo di moltiplicazione.

pprint_polynomial(Polynomial).


%%%% end of file -- polinomimultivariati.pl --
