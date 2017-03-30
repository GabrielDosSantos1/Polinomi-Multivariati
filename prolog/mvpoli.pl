%%%% 797785 Dos Santos Gabriel
%%%% no collaborazioni

%% %%%% -*- Mode: Prolog -*-
%% %%%% mvpoli.pl

%is_polynomial(Polynomial, Poly) 
% è vero quando Polynomial è un Poly.
is_polynomial(poly(Monomials), poly(MonomialsSorted)) :-
    is_list(Monomials),
    is_MonomialList(Monomials, M),
    make_polynomial(M, MonomialsSorted),!.

is_polynomial(Expression, Polynomial):-
    as_polynomial(Expression, Polynomial),!.

is_polynomial(Monomial, poly([M])):-
    is_monomial(Monomial, M).

%is_MonomialList(MonomialList, MList).
% è vero quando MonomialList è una lista di monomi.
is_MonomialList([], []).
is_MonomialList([Monomial| Rest], [M| Ms]):-
    is_MonomialList(Rest, Ms),
    is_monomial(Monomial, M).


%is_monomial(Monomial, M)
%è vero quando Monomial è un monomio.
is_monomial(m(0, _, _), m(0, 0, [])).
is_monomial(m(C, TD, VPs), m(C, TD, VPsSorted)):-
    integer(TD),
    TD >= 0,
    is_list(VPs),
    is_varpowerList(VPs),
    monomial_sort(VPs, VPsSorted).

is_monomial(Expression, Monomial) :-
    as_monomial(Expression, Monomial).

%is_varpowerList(VarpowerList).
% é vero quando Varpower è una VarList.
is_varpowerList([]).
is_varpowerList([VP| VPs]):-
    is_varpower(VP),
    is_varpowerList(VPs).

%is_varpowerList(Varpower).
% é vero quando Varpower è una Var.
is_varpower(v(P, S)):-
    integer(P),
    P >= 0,
    atom(S).

%coefficients(Polynomial, Coeﬃcients) .
%è vero quando Coefficients è una lista dei coefficienti di Polynomial.
coefficients(Polynomial, Coefficients) :-
    is_polynomial(Polynomial, poly(Monomials)),
    find_coefficients(Monomials, Coefficients), !.

%find_coefficients(Monomial, Coefficient)
% è vero quando Coefficient è il coefficiente di Monomial.
find_coefficients([], []).

find_coefficients([m(C, _, _)| Rest], [C| Tail]) :-
    find_coefficients(Rest, Tail).

%variables(Poly, Variables).
%Il predicato variables è vero quando Variables `e una lista dei simboli di 
%variabile che appaiono in Poly
variables(Polynomial, Variables) :-
    is_polynomial(Polynomial, poly(Monomials)),
    find_monomials(Monomials, V),
    duplicates(V, Variables),!.

%find_monomials(MList, VarList).
%il predicato è vero quando VarList  e una lista di variabili dei monomi
%di  MList (una lista di monomi ).

find_monomials([], []).
find_monomials([m(_, _, Var)| Rest], Monomials) :-
    find_variables(Var, Variables),
    find_monomials(Rest, RestMon),
    append(Variables, RestMon, Monomials).

%find_variables(VarList, SymList)
%il predicato è vero quando SymList è una lista dei simboli delle variabili
% di VarList.

find_variables([], []).
find_variables([v(_, Symbol)| Rest], [Symbol| RestSym]) :-
    find_variables(Rest, RestSym).

%duplicates(SymList, SymListNoDuplicates).
%il predicato è vero quando SymListNoDuplicates è una lista di simboli di 
%SymList senza ripetizione dei simboli uguali.
duplicates([], []).
duplicates([X| Rest], [X| NoDuplicates]) :-
    duplicates(Rest, NoDuplicates),
    not(member(X, NoDuplicates)),!.

duplicates([X| Rest], NoDuplicates):-
    duplicates(Rest, NoDuplicates),
    member(X, NoDuplicates),!.

duplicates([X],[X]).

%monomials(Polynomial, Monomials)
%il predicato è vero quando Monomials è una lista ordinata dei monomi che 
%appaiono in Polynomial
monomials(Polynomial, Monomials):-
    is_polynomial(Polynomial, poly(Monomials)).


%maxdegree(Polynomial, Degree).
%Il predicato è vero quando Degree è il massimo grado dei monomi in Polynomial.
maxdegree(Polynomial, MaxDegree):-
    is_polynomial(Polynomial, poly(Monomials)),
    degreeList(Monomials, Degrees),
    max_list(Degrees, MaxDegree).

%mindegree(Polynomial, Degree) .
%il predicato è vero quando Degree è il minimo grado dei monomi in Polynomial
mindegree(Polynomial, Mindegree):-
    is_polynomial(Polynomial, poly(Monomials)),
    degreeList(Monomials, Degrees),
    min_list(Degrees, Mindegree).

%degreeList(MList, DegreeList).
%il predicato è vero quando DegreeList è una lista ordinata dei gradi dei 
%monomi in MList

degreeList([], []).
degreeList([m(_, Degree, _)| Rest], [Degree| Rec]) :-
    degreeList(Rest, Rec).

%polyplus(Polynomial1, Polynomial2, Result).
%il predicato è vero quando Result è la somma di Polynomial1 e Polynomial2.
polyplus(Polynomial1, Polynomial2, Result ) :-
    is_polynomial(Polynomial1, poly(M1)),
    is_polynomial(Polynomial2, poly(M2)),
    append(M1,M2,P),
    is_polynomial(poly(P), Result), !.

%polyminus(Polynomial1, Polynomial2, Result) 
%il predicato è vero quando Result è la differenza tra Polynomial1 e 
%Polynomial2.
polyminus(Polynomial1, Polynomial2, Result) :-
    is_polynomial(Polynomial1, P1),
    is_polynomial(Polynomial2, P2),
    polytimes(poly([m(-1, 0, [])]), P2, P2PerMinusOne),
    polyplus(P1, P2PerMinusOne, Result), !.

%polytimes
%polytimes(Polynomial1, Polynomial2, Result) 
%il predicato è vero quando Result è la moltiplicazione tra Polynomial1 e 
%Polynomial2.

polytimes(Polynomial1, Polynomial2, Result) :-
    is_polynomial(Polynomial1, poly(M1)),
    is_polynomial(Polynomial2, poly(M2)),
    dotProduct(M1, M2, M1PM2),
    is_polynomial(poly(M1PM2), Result), !.

%dotProduct(MList1, MList2, Result)
%il predicato è vero quando Result (una lista di monomi) è il risultato della 
%moltiplicazione di ogni elemento della lista di monomi MList1 per ogni 
%elemento elemento della lista di monomi MList2. 
%(dot product perché è l’unificazione della chiamata ricorsiva del prodotto 
%scalare)

dotProduct([], _, []).
dotProduct([Monomial| Rest], Monomials, Polytimes) :-
    product(Monomial, Monomials, Solution),
    dotProduct(Rest, Monomials, RicSolution),
    append(Solution, RicSolution, Polytimes).

%product(Monomio, MList, Result).
%il predicato è vero quando Result (una lista di monomi) è il risultato della 
%moltiplicazione del Monomio per ogni elemento di MList.

product(m(_, _, _), [], []).
product(m(C1, TD1, Var1), [m(C2, TD2, Var2)| Rest], [m(C, TD, Var)| Rec]) :-
    C is C1 * C2,
    TD is TD1 + TD2,
    append(Var1, Var2, Var),
    product(m(C1, TD1, Var1), Rest, Rec).

%as_monomials
%as_monomial(Expression, Monomio)
%il predicato è vero quando Monomio(struttura interna) `e il termine che 
%rappresenta il polinomio risultante dal “parsing” dell’espressione Expression.
as_monomial(Expression, Monomial) :-
    as_variable(Expression, Variables),
    find_coefficients(Variables, Coe, ListVar),
    C is round(Coe * 1000) / 1000,
    sumdegree(ListVar, TD),
    is_monomial(m(C, TD, ListVar), Monomial),!.

%as_variable
%as_variable(VarExpression, Var)
%il predicato è vero quando Var (struttura interna) `e il termine che 
%rappresenta il polinomio risultante dal “parsing” dell’espressione 
%VarExpression.
as_variable(X*Y, Solution) :-
    as_variable(X, R), !,
    as_variable(Y, E),
    append(R, E, Solution).

as_variable(X^Y, [v(Y,X)]) :-
    Y > 0,
    integer(Y),
    X \= pi,
    atom(X), !.

as_variable(+X^Y, [v(Y,X),1]) :-
    Y > 0,
    integer(Y),
    X \= pi,
    atom(X), !.

as_variable(-X^Y, [v(Y,X),-1]) :-
    Y > 0,
    integer(Y),
    X \= pi,
    atom(X), !.

as_variable(0, [0]).
as_variable(X, [v(1,X)]):- atom(X), X \=  pi.
as_variable(X^0,[1]) :- atom(X), X \=  pi.
as_variable(+X, [v(1,X)]):- atom(X), X \=  pi.
as_variable(+X^0,[1]) :- atom(X), X \=  pi.
as_variable(-X, [v(1,X), -1]):- atom(X), X \=  pi.
as_variable(-X^0,[-1]) :- atom(X), X \=  pi.
as_variable(C, R) :- as_coefficient(C, R).

%find_coefficient
%find_coefficients(List ,  Coefficient, VarList)
%il predicato è vero quando Coefficient è la moltiplicazione di tutti i numeri 
%di List e VarList è una lista con tutte le variabili Var di List.

find_coefficients([], 1, []).
find_coefficients([0| _], 0, []).
find_coefficients([Coefficient| Rest], Product, Variables) :-
    number(Coefficient), !,
    find_coefficients(Rest, RecSol, Variables),
    Product is Coefficient * RecSol.
find_coefficients([C| Rest], Product, [C| Variables]) :-
    find_coefficients(Rest, Product, Variables).

%sumdegree
%sumdegree(VarList, Sum).
%il predicato è vero quando sum è la somma dei gradi di VarList
sumdegree([], 0).
sumdegree([v(Degree, _)| Rest], Solution) :-
    sumdegree(Rest, RecSol),
    Solution is Degree + RecSol.

%ordinamento dei monomi
%monomial_sort(VarList , SMonomial)
%il predicato è vero quando SMonomial è una lista ordinata delle variabili di 
%VarList
monomial_sort([], []).
monomial_sort([Variable], [Variable]).
monomial_sort(Monomials, Sorted) :-
    Monomials = [_, _| _],
    divide(Monomials, Monomial1, Monomial2),
    monomial_sort(Monomial1, Sorted1),
    monomial_sort(Monomial2, Sorted2),
    monomial_compare(Sorted1, Sorted2, Sorted), !.

%monomial_compare(Var1, Var2, Result)
%il predicato è vero quando :
%Result è uguale a 1 se Var1 > Var2
%Result è uguale a 0 se Var1 = Var2
%Result è uguale a -1 se Var1 < Var2	

monomial_compare([], Variable, Variable).
monomial_compare(Variable, [], Variable) :- Variable \= [].
monomial_compare([v(G1,S1)|T1],[v(G2,S2)|T2],[v(G1,S1)|Tail]) :-
    atom_codes(S1, C1),
    atom_codes(S2, C2),
    majorXThanY(C1, C2, -1),
    monomial_compare(T1,[v(G2,S2)|T2],Tail).

monomial_compare([v(G1,S1)|T1],[v(G2,S2)|T2],[v(G2,S2)|Tail]) :-
    atom_codes(S1, C1),
    atom_codes(S2, C2),
    majorXThanY(C1, C2, 1),
    monomial_compare([v(G1,S1)|T1],T2,Tail).

monomial_compare([v(G1,S1)|T1],[v(G2,S2)|T2],[v(Sum,S2)|Tail]):-
    atom_codes(S1, C1),
    atom_codes(S2, C2),
    majorXThanY(C1, C2, 0), %fare il compare
    Sum is G1 + G2,
    monomial_compare(T1,T2,Tail).

%majorXThanY(SymbolX, SymbolY, Result)
%il predicato è vero quando :
%Result è uguale a 1 se SymbolX > SymbolY
%Result è uguale a 0 se SymbolX = SymbolY
%Result è uguale a -1 se SymbolX < SymbolY

majorXThanY([], [], 0).
majorXThanY([X| _], [Y| _], -1):-
    X < Y.
majorXThanY([X| _], [Y| _], 1):-
    X > Y.
majorXThanY([X| Xs], [Y| Ys],Result):-
    X = Y,
    majorXThanY(Xs, Ys, Result).

%divide
divide(L,A,B) :- d(L,L,A,B).
d([],R,[],R).
d([_],R,[],R).
d([_,_|T],[X|L],[X|L1],R) :- d(T,L,L1,R).

%coefficient
%as_coefficient(Coeficient, Number)
%il predicato è vero quando Number è una coefficient di un monomio
as_coefficient(C, [R]) :-
    catch(R is C, _,fail).
as_coefficient(C^E, [R]) :-
    is_number(C, [C1]),
    is_number(E, [E1]),
    R is C1^E1,!.
as_coefficient(C/D, [R]) :-
    is_number(C, [C1]),
    is_number(D, [D1]),
    R is C1 rdiv D1, !.

%is_number(Number1, Number2)
%il predicato è vero quando Number2 è una numero reale risultante da Number1 
is_number(C, [C]) :- number(C), !.
is_number(C, R) :- as_coefficient(C,R), !.

%as_polynomial(Expression, Poly)
%Il predicato `e vero quando Polynomial `e il termine che rappresenta il 
%polinomio risultante dal “parsing” dell’espressione Expression.
as_polynomial(Expression, Polynomial) :-
    get_Monomials(Expression, Monomials),
    is_polynomial(poly(Monomials), Polynomial), !.


%get_Monomials(Expression, Monomials)
%il predicato è vero quando Monomials è una lista di monomi risultanti dal 
%“parsing” dell’espressione Expression 
get_Monomials(Expression1+Expression2, Monomial) :-
    as_monomial(Expression2, Monomial1),
    get_Monomials(Expression1, Monomial2),
    append(Monomial2, [Monomial1], Monomial).

get_Monomials(Expression1-Expression2, Monomial) :-
    as_monomial(Expression2, Monomial2),
    Monomial2 = m(Coefficient, Power, Variables),
    CoeNeg is Coefficient * -1,
    get_Monomials(Expression1, Monomial1),
    append(Monomial1, [m(CoeNeg, Power, Variables)], Monomial).

get_Monomials(Expression, [Monomial]) :-
    as_monomial(Expression, Monomial).

%make_polynomial(MList SMList)
%il predicato è vero quando SMList è una lista di monomi ordinato
make_polynomial(Monomial, Result):-
    polynomial_sort(Monomial, S1),
    sort(2, @=< , S1, S2),
    reduce_polynomial(S2, Result).

%reduce_polynomial
%reduce_polynomial(MonomialList, MList)
%il predicato è vero quando Mlist è MonomialList priva dei monomi con 
%coefficienti uguali a 0
reduce_polynomial([], []).
reduce_polynomial([m(0, _, _)| Ms], RMs):-
    reduce_polynomial(Ms, RMs).
reduce_polynomial([M| Ms], [M| RMs]) :-
    reduce_polynomial(Ms, RMs).

%polynomial_sort(MonomialList1 , MonomialList2 MonomialListSorted)
%Il predicato è vero quando MonomialList,Sorted è l’unione di MonomialList1 
%e MonomialList2 ordinata in ordine lessicografico
polynomial_sort([], []).
polynomial_sort([Monomial], [Monomial]).
polynomial_sort(Monomials, Sorted) :-
    Monomials = [_, _| _],
    divide(Monomials, Monomial1, Monomial2),
    polynomial_sort(Monomial1, Sorted1),
    polynomial_sort(Monomial2, Sorted2),
    lexical_order(Sorted1, Sorted2, Sorted).


%lexical_order(Monomial1, Monomial1)
%Il predicato è vero quando MonomialListSorted e una lista con Monomial1 e 
%Monomial2 ordinata in ordine lessicografico

lexical_order([], Monomial, Monomial).
lexical_order(Monomial, [], Monomial) :- Monomial \= [].
lexical_order([M1| T1], [M2| T2], [M1| Tail]):-
    compare_rules(M1, M2, -1),
    lexical_order(T1, [M2| T2], Tail).

lexical_order([M1| T1], [M2| T2], [M2 |T]) :-
    compare_rules(M1, M2, 1),
    lexical_order([M1| T1], T2, T).

lexical_order([m(C1, TD, V)| T1],[m(C2, TD, V)| T2],[m(Sum, TD, V)| Tail]):-
    Sum is C1 + C2,
    lexical_order(T1, T2, Tail).

lexical_order([M1| T1], [M2| T2], [M1, M2| Tail]):-
    compare_rules(M1, M2, 0),
    lexical_order(T1, T2, Tail).


%compare_rules(Monomial1, Monomial2, Result)
%il predicato è vero quando :
%Result è uguale a 1 se Monomial1> Monomial2
%Result è uguale a 0 se Monomial1= Monomial2
%Result è uguale a -1 se Monomial1< Monomial2
compare_rules(m(_, _, [v(G, S)| T1]), m(_, _, [v(G, S)| T2]), Tail) :-
    compare_rules(m(_, _, T1), m(_, _, T2), Tail).
compare_rules(m(_, _, []),m(_, _, []), 0).

compare_rules(m(_, _, [v(_, S1)| _]), m(_, _, [v(_, S2)| _]), -1) :-
    atom_codes(S1, C1),
    atom_codes(S2, C2),
    majorXThanY(C1, C2, -1).

compare_rules(m(_, _, [v(G1, S)| _]), m(_, _, [v(G2, S)| _]), -1) :-
    G1 < G2.
compare_rules(m(_, _, []), m(_, _, M), -1):- M \= [].

compare_rules(m(_, _, [v(_, S1)| _]), m(_, _, [v(_, S2)| _]), 1) :-
    atom_codes(S1, C1),
    atom_codes(S2, C2),
    majorXThanY(C1, C2, 1).

compare_rules(m(_, _, [v(G1, S)| _]), m(_, _, [v(G2, S)| _]), 1) :-
    G1 > G2.
compare_rules(m(_, _, M),m(_, _, []), 1) :- M \= [].

%pprint_polynomial(Poly)
%Il predicato pprint_polynomial risulta vero dopo aver stampato (sullo 
%“standard output”) una rappresentazione tradizionale del termine polinomio 
%associato a Poly.
pprint_polynomial(poly(Monomial)):-
    print_monomial(Monomial, ListChars),
    atomics_to_string(ListChars, Expression),
    write(Expression).

%print_monomial(MonomialList, MList )
%Il predicato è vero quando MList è una lista in cui ogni elemento è un termine
%della rappresentazione tradizionale dei monomi associati a MonomialList. 

print_monomial([],[]).
print_monomial([m(C, _, Variables)|Rest], Result):-
    C >= 0,!,
	 print_variables(Variables, Var),
	 print_monomial(Rest, Monomial),
	 append([+,C], Var , Vars),
	 append(Vars, Monomial, Result).

print_monomial([m(C, _, Variables)|Rest], Result):-
    C < 0,!,
	print_variables(Variables, Var),
	print_monomial(Rest, Monomial),
	append([C], Var , Vars),
	append(Vars, Monomial, Result).

%print_variables(Var, VarList)
%Il predicato è vero quando VarList è una lista in cui ogni elemento è un 
%termine della rappresentazione tradizionale del termine polinomio associato a 
%Poly. 
print_variables([],[]).
print_variables([v(1, Symbol)|Rest], [*,Symbol| R]) :-
    print_variables(Rest, R), !.
print_variables([v(TD, Symbol)|Rest], [*,Symbol,^,TD|R]) :-
    print_variables(Rest,R).

%polyvall(Polynomial, VariableValues, Value) 
%Il predicato polyval `e vero quanto Value contiene il valore del polinomio 
%Polynomial dove ogni variabile ha associato un valore.

polyval(Polynomial, VariableValues, Value):-
    is_polynomial(Polynomial, P),
    variables(P, Variables),
    accoppia(Variables, VariableValues, List),
    risolutore_polinomio(P, List, Value),!.

risolutore_polinomio(poly([]), _ , 0).
risolutore_polinomio(poly([M1]), List, Soluzione):-
    risolutore_monomio(M1, List, Soluzione).

risolutore_polinomio(poly([M1| Rest]), List, Soluzione):-
    risolutore_polinomio(poly(Rest), List, RSoluzione),
    risolutore_monomio(M1, List, MSoluzione),
    Soluzione is MSoluzione + RSoluzione.

risolutore_monomio(m(Coe,_, Var), List, Soluzione):-
    risolutore_variabili(Var, List, ValoreVariabili),
    Soluzione is Coe*ValoreVariabili.

risolutore_variabili([], _, 1).
risolutore_variabili([v(Grado, Simbolo)|Resto], List, Soluzione):-
    risolutore_variabili(Resto, List, RSoluzione),
    member((Simbolo, Valore), List),
    SoluzioneParziale is Valore^Grado,
    Soluzione is SoluzioneParziale*RSoluzione.

risolutore_variabili([v(Grado,Simbolo)], List, Soluzione):-
    member((Simbolo, Valore), List),
    Soluzione is Valore^Grado.

accoppia([X| XRest], [Y| YRest], [(X, Y)| Rest]) :-
	accoppia(XRest, YRest, Rest).
accoppia([X], [Y], [(X, Y)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% End of file -- mvpoli.pl --
