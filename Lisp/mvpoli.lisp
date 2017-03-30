;;;; 797785 Dos Santos Gabriel
;;;; no  collaborazioni

;is-polynomial
;is_polynomial Polynomial -> Poly 
; Data una struttura Polynomial ritorna un Poly
;è vero quando Polynomial è un Poly.
(defun is-polynomial (poly) 
  (cond ((and (listp poly) 
	      (eq (car poly) 'poly) 
	      (listp (second poly))) 
	 (cond ((eq nil (second poly)) 
		(list 'poly nil)) 
	       (T (list 'poly (polynomial-sort
			       (is-monomials-poly (second poly))))))) 
	((or (and (listp poly) 
		   (eq (car poly) 'm)) 
	      (and (listp poly) 
		   (eq (car poly) 'v))) 
	  (is-polynomial (list 'poly (list (is-monomial poly))))) 
	((equal '(POLY NIL) poly) poly) 
	((and (listp poly) 
	      (eq '+ (car poly))) 
	 (as-polynomial poly)) 
	(T (error "Polinomio non valido")))) 
	
;is-monomials-poly
;Data una struttura Monomials (una lista di monomi) ritorna una dei monomi 
(defun is-monomials-poly (monomials) 
  (let ((var (list (is-monomial (car monomials))))) 
    (cond ((eq nil (cdr monomials)) var) 
	  ((eq 0 (second (car var))) 
	   (is-monomials-poly (cdr monomials))) 
	  (T (append var (is-monomials-poly (cdr monomials))))))) 
		
;is-varpowers
;Data una struttura vp ritorna nil se vp non è una struttura varpower 
;T altrimenti
(defun is-varpowers(vp) 
  (and (listp vp) 
       (eq 'v (first vp)) 
       (let ((p (second vp)) 
	     (v (third vp))) 
	 (and (integerp p) 
	      (>= p 0) 
	      (symbolp v))))) 
		  
;is-monomial expression-> monomial
;Data una struttura monomial ritorna un monomial secondo la rappresentazione 
;interna
(defun is-monomial (monomial) 
  (cond ((and (listp monomial) 
	      (eq 'm (car monomial)) ; m
	      (numberp (second monomial)) ; # coeffiente 
	      (integerp (third monomial)) ; # grado 
	      (>= (third monomial) 0) 
	      (= (get-Total-Degree-Variables (fourth monomial)) (third monomial))) 
	 (ordinamento-monomial (list 'm
				     (second monomial) 
				     (get-Total-Degree-Variables (fourth monomial)) 
				     (fourth monomial)))) 
	((monomial-vars-and-powers (list monomial)) 
	 (ordinamento-monomial (list 'm
				     1
				     (get-Total-Degree-Variables (fourth monomial)) 
				     monomial))) 
	((and (listp monomial) 
	      (or (eq '* (car monomial)) 
		  (numberp monomial) 
		  (eq 'expt (car monomial)))) 
	 (as-monomial monomial)) 
	(T (error "monomio non valido")))) 
	
;monomial-vars-and-powers
;data una struttura monomial-vars-and-powers effetua una chiamata ricorsiva a 
;is-varpower
(defun monomial-vars-and-powers (vars-and-power) 
  (let ((var (cond (
		    (not (eq (car vars-and-power) nil)) 
		    (is-varpowers (car vars-and-power))) 
		   (T nil)))) 
    (cond ((not (eq nil var)) 
	   (cond ((eq (cdr vars-and-power) nil) var) 
		 (T (and var 
			 (monomial-vars-and-powers (cdr vars-and-power))))))))) 
;varpowers
;Data una struttura Monomial, ritorna la lista di varpowers VP-list.
(defun varpowers (monomial) 
  (let ((m (is-monomial monomial))) 
    (fourth m))) 

;var-of
;Data una struttura Monomial, ritorna la lista di variabili Variables.
(defun vars-of (monomial) 
  (let ((m (is-monomial monomial))) 
    (get-symbol (varpowers m)))) 
	
;get-symbol
;Data una struttura var-and-power restituice una lista di simboli
(defun get-symbol (var-and-power) 
  (let ((symbol (third (car var-and-power)))) 
    (cond ((eq nil symbol) nil) 
	  ((eq nil(cdr var-and-power)) 
	   (list symbol)) 
	  (T (append (list symbol) (get-symbol(cdr var-and-power))))))) 

;monomial-degree
;Data una struttura Monomial, ritorna il suo grado totale TotalDegree.
(defun monomial-degree (monomial) 
  (let ((m (is-monomial monomial))) 
    (get-Total-Degree-Variables (varpowers m)))) 

;get-Total-Degree-Variables
;Data una struttuta variables ritorna la somma dei gradi delle variabili
(defun get-Total-Degree-Variables (variables) 
  (let ((degree
	 (cond ((eq nil variables) 0) 
	       (T (second (car variables)))))) 
    (cond ((eq nil (cdr variables)) degree) 
	  (T (+ degree (get-Total-Degree-Variables (cdr variables))))))) 

;monomial-Coefficient
;Data una struttura Monomial, ritorna il suo coefficiente Coefficient.
(defun monomial-Coefficient (monomial) 
  (let ((m (is-monomial monomial))) 
    (second m))) 

;coefficients
;La funzione coefficients ritorna una lista Coefficients dei 
;coefficienti di Poly.
(defun coefficients (poly) 
  (let ((polynomial (is-polynomial poly))) 
    (let ((coe
	   (cond ((eq nil (car (second polynomial))) 
		  (list 0)) 
		 (T (list (monomial-Coefficient (car (second polynomial)))))))) 
      (cond((eq nil(cdr(second polynomial))) coe) 
	   (T
	    (append coe (coefficients (list 'poly (cdr (second polynomial)))))))))) 

;reduce-variables
;data una struttura variables ritorna variables senza duplicati
(defun reduce-variables (variables noduplicates)
  (cond ((eq nil variables) noduplicates)	
	((not (eq nil (car variables))) 
	 (cond
	  ((is-member (car variables) noduplicates)
	   (reduce-variables (cdr variables) noduplicates))
	  ((not (is-member (car variables) noduplicates))
	   (reduce-variables
	    (cdr variables)
	    (append noduplicates (list (car variables)))))))
	(T (error "Errore nella funzione reduce-variable"))))

;is-member
;dato un elemento e una lista ritorna T se l'elemento è presente nella lista 
;altrimenti nil
(defun is-member (x lista)
  (cond ((eq nil lista) nil)
	((equal x (car lista)) T)
	((not (equal x (car lista)))
	 (is-member x (cdr lista)))
	(T (error "Errore nella funzione is-member"))))

;variables
;La funzione variables ritorna una lista Variables dei simboli di variabile che 
;appaiono in Poly.
(defun variables (poly)
  (let ((m (monomials poly)))
    (reduce-variables (find-variables m) '())))

;find-variables
;Data una stuttura monomial la funzione ritorna una lista dei simboli delle 
;variabili del monomio
(defun find-variables (m)
  (cond ((eq nil (car m)) nil)
   ((not (eq nil (car m)))
    (append (vars-of (car m)) (find-variables (cdr m))))
   (T (error "errore nella funzione find-variables"))))

;monomials
;La funzione monomials ritorna la lista – ordinata, si veda sotto – dei monomi 
;che appaiono in Poly.
(defun monomials (poly) 
  (let ((polynomial (is-polynomial poly))) 
    (second polynomial)))

;maxdegree
;La funzione maxdegree ritorna il massimo grado dei monomi che appaiono in Poly.
(defun maxdegree (poly) 
  (let ((m (monomials poly))) 
    (apply 'min (get-degrees m))))

;mindegree
;La funzione mindegree ritorna il minimo grado dei monomi che appaiono in Poly.
(defun mindegree (poly) 
  (let ((m (monomials poly))) 
    (apply 'min (get-degrees m))))

; get-degrees
; la funzione ritorna una lista dei gradi dei monomi
(defun get-degrees (monomials) 
  (let ((degree
	 (cond ((eq nil monomials) 0) 
	       (T (monomial-degree (car monomials)))))) 
    (cond ((eq nil(cdr Monomials)) (list degree)) 
	  (T (append degree (get-degrees(cdr monomials)))))))

; polyplus
; La funzione polyplus produce il polinomio somma di Poly1 e Poly2.
(defun polyplus (poly1 poly2) 
  (let ((polynomial1 (is-polynomial poly1)) 
	(polynomial2 (is-polynomial poly2))) 
    (is-polynomial (list
		    'poly
		    (append (second polynomial1) (second polynomial2))))))

;polyminus
;La funzione polyplus produce il polinomio di?erenza di Poly1 e Poly2.
(defun polyminus (poly1 poly2)
  (let((p1 (is-polynomial poly1))
       (p2 (is-polynomial poly2)))
    (polyplus p1 (polytimes p2 '(POLY((M -1 0 NIL)))))))

;polytimes
;La funzione polytimes ritorna il polinomio risultante dalla moltiplicazione di 
;Poly1 e Poly2.
(defun polytimes (poly1 poly2) 
  (let ((m1 (monomials poly1)) 
	(m2 (monomials poly2))) 
    (cond ((or (eq nil m1) 
	       (eq nil m2)) 
	   (list 'poly nil)) 
	  (T (is-polynomial (list 'poly(product m1 m2))))))) 
	  
;product
; data una struttura monomial1 e monomial2 la funzione ritorna il risultato 
; della moltiplicazione di ogni elemento di monomial1 per ogni elemento di 
; monomial2 
(defun product (monomial1 monomial2) 
  (let ((monomial (dot-product(car monomial1) monomial2))) 
    (cond ((eq nil (cdr monomial1)) monomial) 
	  (T (append monomial (product (cdr monomial1) monomial2)))))) 

;dot-product
; data una struttura monomial e una listmonomials la funzione ritorna il 
; risultato della moltiplicazione di monomial per ogni elemento di listmonomials
(defun dot-product (monomial listmonomials) 
  (let ((coefficient
	 (*
	  (car (coefficients monomial)) 
	  (car (coefficients (car listmonomials))))) 
	(totalDegree
	 (+
	  (monomial-degree monomial) 
	  (monomial-degree (car listmonomials)))) 
	(variables
	 (append
	  (varpowers monomial) 
	  (varpowers (car listmonomials))))) 
    (cond ((eq nil (cdr listmonomials)) 
	   (list (list 'm coefficient totalDegree variables))) 
	  (T
	   (append 
	    (list (list 'm coefficient totalDegree variables)) 
	    (dot-product monomial (cdr listmonomials))))))) 
;print-list
;data una lista la funzione stampa ogni elemento della lista
(defun print-list (l)
  (cond ((eq nil (car l))
	 nil)
	(T (and (princ (car l))
		(print-list (cdr l))))))

;pprint-polynomial
;data una struttura poly la funzione ritorna una rappresentazione 
;tradizionale di poly tramite lo output standard 
(defun pprint-polynomial(poly)
  (let ((ms 
	 (print-polynomial(monomials poly))))
    (print-list ms)))

;print-polynomial
;data una struttura polynomial la funzione ritorna una lista della 
;rappresentazione tradizionale di polynomial
(defun print-polynomial(monomial)
  (cond((eq nil monomial)
	'()) 
       ((eq nil(cdr monomial))
	(print-monomial(car monomial)))
       ( T 
	 (append(print-monomial(car monomial))(print-polynomial(cdr monomial))))))

;print-monomial
;data una struttura monomial la funzione ritorna una lista della 
;rappresentazione tradizionale di monomial
(defun print-monomial(monomial)
  (let ((coefficient 
	 (list(monomial-coefficient monomial)))
	(variables 
	 (append(print-variable(varpowers monomial)))))
    (cond((=(car coefficient) 1) 
	  (append '(+) variables))
	 ((=(car coefficient) -1) 
	  (append '(-) variables))
	 ((or(/=(car coefficient) -1)(/=(car coefficient) 1))
	  (cond((>(car coefficient) 1)
		(cond((eq nil variables) 
		      (append '(+) coefficient))
		     (T
		      (append '(+)(append Coefficient(append '(*) variables)))))) 
	       (T 
		(append Coefficient(append '(*) variables))))))))
		
;print-variables
;data una struttura variables la funzione ritorna una lista della 
;rappresentazione tradizionale di variables
(defun print-variable(variables)
  (let ((var (cond ((eq nil variables)
		    '())
		   ((= 1(second(car variables)))
		    (list(third(car variables))))
		   ((< 1(second(car variables))) 
		    (list(third(car variables)) '^(second(car variables)))))))
    (cond ((eq nil(cdr variables)) 
	   var )
	  ( T 
	    (append 
	     (append 
	      var 
	      (list '*)) 
	     (print-variable(cdr variables)))))))

;as-monomial
;La funzione as-monomial ritorna la struttura dati (lista) che rappresenta il 
;monomio risultante dal “parsing” dell’espressione Expression
(defun as-monomial (expression) 
  (let ((coefficient
	 (cond ( ; caso(* ....) 
		(and (listp expression) 
		     (eq '* (car expression))) 
		(get-coefficient (cdr expression) 1)) 
	       ( ; caso solo -4
		(and (numberp expression)) 
		expression) 
	       ((and (listp expression) 
		     (eq 'expt (car expression))) 
		1))) 
					;(condizione2) ; caso in cui c'e solo un simbolo tipo x)) 
	(variables
	 (cond ( ; caso(* ....) 
		(and (listp expression) 
		     (eq '* (car expression))) 
		(get-variables (cdr expression) '())) 
	       ( ; caso solo -4
		(and (numberp expression)) 
		'()) 
	       ((and (listp expression) 
		     (eq 'expt (car expression))) 
		(get-variables (list expression) '())) 
					;(condizione2) ; caso in cui c'e solo un simbolo tipo x 
	       ))) 
    (cond ((= 0 coefficient) 
           (is-monomial (list 'm coefficient 0 nil))) 
          (T 
           (is-monomial (list 'm
			      coefficient
			      (get-Total-Degree-Variables variables)
			      variables)))))) 

;get-coefficient
;data una struttura expression ritorna un coefficiente 
(defun get-coefficient (expression s) 
  (let (
	(coefficient
	 (cond ( ; CONDIZIONE1 siamo nel caso( 1 ....) 
		(and (listp expression) 
		     (numberp (car expression))) 
		(* s (car expression))) 
	       ( 
		   ; CONDIZIONE2 siamo nel caso(* ...(4) ...) ALTENSIONE 
		   ;s*-4*s non è un monomio
		(and ; s*(-4) *x è un monomio
		 (listp expression) 
		 (listp (car expression)) 
		 (numberp (caar expression))) 
		(* s (caar expression))) 
	       ( ; CONDIZIONE3 siamo nel caso non è un numero
		T s)))) 
    (cond ( ; CONDIZIONE1 non ci sono piu elementi da analizzare
	   (eq nil (cdr expression)) 
	   coefficient) 
	  ( ; CONDIZIONE2 bisogna fare una chiamata ricorsiva
	   T (get-coefficient (cdr expression) coefficient))))) 

;get-variables
;data una struttura espression la funzione ritorna una lista delle variabili 
;(struttura interna) di expression
(defun get-variables (expression variables) 
  (let((var
        (cond( 
		; CONDIZIONE1 ho una lista quindi puo essere solo un esponente tipo
		;(expt 4 s) # funziona
              (listp(car expression)) 
              (cond ((eq(car(as-exponent(car expression))) nil) 
                     nil) 
                    (T(list(as-exponent(car expression)))))) 
             ( ; CONDIZIONE2 ho una variabile tipo s# funziona
              (symbolp(car expression)) 
              (list (list 'v 1 (car expression)))) 
             ( ; CONDIZIONE3 ho un NUMERO # funziona
              (numberp (car expression)) nil)))) 
    (cond((eq nil (cdr expression)) 
          (append variables var) 
          ) ; non ci sono piu elementi
         (T(get-variables (cdr expression) (append variables var)) 
           ) ; ci sono altri elementi
         ))) 

;as-exponent
;data un struttura exponent la funzione ritorna una variabile
(defun as-exponent (exponent) 
  (cond( ; CONDIZIONE1 è della forma(expt 5 a) # funziona
        (and(eq 'expt(car exponent)) 
            (numberp(third exponent)) 
            (>(third exponent) 0) 
            (symbolp(second exponent))) 
        (list 'v(third exponent) (second exponent))) 
       ( ; CONDIZIONE2 input è nella forma(expt 0 s) # funziona
        (and(eq 'expt(car exponent)) 
            (numberp(third exponent)) 
            (=(third exponent) 0) 
            (symbolp(second exponent))) 
        '()))) 

;as-polynomail
;La funzione as-polynomial ritorna la struttura dati (lista) che rappresenta il 
;monomio risultante dal “parsing” dell’espressione Expression
(defun as-polynomial (expression) 
  (let((monomials
        (cond((and(listp expression) 
                  (eq '+(car expression)) 
                  (listp(cdr expression))) 
              (get-monomials(cdr expression))) 
	     ((or (and (listp expression) 
		       (eq '*(car expression))) 
		  (and (numberp expression)) 
		  (and (listp expression) 
		       (eq 'expt(car expression)))) 
	      (list 'poly (list (as-monomial expression))))))) 
    (is-polynomial(list 'poly monomials))))

;get-monomials
;data una struttura expression la funzione ritorna il coefficiente di 
;expression
(defun get-monomials(expression) 
  (cond((eq nil(car expression)) 
        '()) 
       ((eq nil(cdr expression)) 
        (list(as-monomial(car expression)))) 
       ((not(eq nil(cdr expression))) 
        (append(list(as-monomial(car expression)))
	       (get-monomials(cdr expression)))))) 

;ordinamento-monomial
;data una struttura monomail la funzione ritorna un monomio ordinato
(defun ordinamento-monomial(monomial) 
  (let((coe
        (second monomial)) 
       (total-degree
        (third monomial)) 
       (var
        (sort(fourth monomial) #'string-lessp :key 'caddr)) 
       ) ; qui va il reduce che racoglie i monomi simili
    (list 'm coe total-degree(monomial-reduce var)))) 

;monomial-reduce
;data una struttura var la funzione ritorna un monomio semplificato ai minimi 
;termini 
(defun monomial-reduce(var) 
  (cond((eq nil(car var)) 
        '()) 
       ((eq(third(car var)) (third(second var))) 
        (monomial-reduce 
         (append 
          (list(list 'v
		     (+(second(car var))
		       (second(second var)))
		     (third(second var)))) 
          (cdr(cdr var))))) 
       ((not(eq(third(car var)) (third(second var)))) 
        (append 
         (list(car var)) 
         (monomial-reduce(cdr var)))))) 

;polinomial-reduce
;data una struttura monomialList la funzione ritorna un polinomio semplificato 
;ai minimi termini
(defun polinomial-reduce(monomialList) 
  (cond ((eq nil(car monomialList)) 
         '()) 
        ((equal(second(car monomialList)) 0) 
         (polinomial-reduce(cdr monomialList))) 
        ((eq nil(second monomialList)) 
         monomialList) 
        ((equal(fourth(car monomialList)) (fourth(second monomialList))) 
         (polinomial-reduce
          (append
           (list(list 'm 
                      (+(second(car monomialList))
			(second(second monomialList))) 
                      (third(car monomialList)) 
                      (fourth(car monomialList)))) 
           (cdr(cdr monomialList))))) 
        ((not(equal(fourth(car monomialList))
		   (fourth(second monomialList)))) 
         (append(list(car monomialList))
		(polinomial-reduce(cdr monomialList)))))) 

;polynomial-sort
;data una struttura monomial la funzione ritorna un monomio ordinato
(defun polynomial-sort(monomial) 
  (cond ((and (eq nil(cdr monomial)) 
              (eq nil(varpowers(car monomial)))) 
         monomial) 
        (T(polinomial-reduce
           (sort 
            (sort monomial 
                  #'compare-rules 
                  :key 'fourth) 
            #'<
            :key 'third))))) 

;compare-rules
;dato varlist1 e varlist2 la funzione ritorna T se varlist1 > varlist2 
;nil altrimenti
(defun compare-rules(varList1 varList2) 
  (let((v1(car varList1)) 
       (v2(car varList2))) 
    (cond( ; condizione in cui la lista è vuota
          (and(eq nil v1) 
              (eq nil v2)) 
          nil) 
         ((and(not(eq nil v1)) 
              (eq nil v2)) 
          nil) 
         ((and(eq nil v1) 
              (not(eq nil v2))) 
          T) 
         ( ; condizione in il simbolo v1 < del simbolo v2
          (string<(third v1) (third v2)) 
          T) 
         ( ; condizione in il simbolo v1 < del simbolo v2
          (string>(third v1) (third v2)) nil) 
         ((and(equal(third v1) (third v2)) 
              (>(second v1) (second v2))) 
          nil) 
         ((and(equal(third v1) (third v2)) 
              (<(second v1) (second v2))) 
          T) 
         ((equal v1 v2) 
          (compare-rules(cdr varList1) (cdr varList2)))))) 

; polyval
;La funzione polyval restituisce il valore Value del polinomio Polynomial 
;nel punto n-dimensionale rappresentato dalla lista VariableValues, 
;che contiene un valore per ogni variabile ottenuta con la funzione variables.
(defun polyval (polynomial variableValues) 
  (let ((p (is-polynomial polynomial))) 
    (risolutore_polinomio (monomials p) 
			  (accoppia (variables p) variableValues)))) 

;risolutore_polinomio
;data una struttura monomials e vv la funzione ritorva il valore reale del
;polinomio
(defun risolutore_polinomio (monomials VV) 
  (cond ((eq nil (car monomials)) 0) 
	((and (not (eq nil (car monomials))) 
	      (eq nil (cdr monomials))) 
	 (risolutore_monomio (car monomials) vv)) 
	((and (not (eq nil (car monomials))) 
	      (not (eq nil (cdr monomials)))) 
	 (+ (risolutore_monomio (car monomials) vv) 
	    (risolutore_polinomio (cdr monomials) vv))))) 
		
;risolutore_monomio 
;data una struttura monomials e vv la funzione ritorva il valore reale del
;polinomio
(defun risolutore_monomio (monomial vv) 
  (let((coe (monomial-coefficient monomial)) 
       (var (risolutore_variabili (varpowers monomial) vv))) 
    (* coe var))) 

;risolutore_variabili
;data una struttura variable e vv la funzione ritorva il valore reale della 
;variabile
(defun risolutore_variabili (variable vv) 
  (let ((grado (second (car variable ))) 
	(valore (find_value (third (car variable)) vv))) 
    (cond ((eq nil (car variable)) 1) 
	  ((and	(not (eq nil (car variable))) 
		(eq nil (cdr variable))) 
	   (expt valore grado)) 
	  ((and	(not (eq nil (car variable))) 
		(not (eq nil (cdr variable)))) 
	   (* (expt valore grado)
	      (risolutore_variabili (cdr variable) vv)))))) 

;accoppia
;data una lista x e una lista y la funzione ritorna una lista composta da una 
;coppia xi yi per ogni elemento di xi e yi
(defun accoppia (x y) 
  (cond ((and (not (eq nil (car x))) 
	      (eq nil (cdr y)) 
	      (not (eq nil (car y))) 
	      (eq nil (cdr x))) 
	 (list (list (car x) (car y)))) 
	((and (not (eq nil (car x))) 
	      (not (eq nil (cdr y))) 
	      (not (eq nil (car y))) 
	      (not (eq nil (cdr x)))) 
	 (append (list (list (car x) (car y))) (accoppia (cdr x) (cdr y)))) 
	( T (error "non è possibile assocciare ad ogni simbolo un valore")))) 

;find_value
;data una struttura simbilo e una listaSimboloValore la funzione ritorna T se 
;simbolo è presente nella lista altrimenti nil
(defun find_value (simbolo listaSimboloValore) 
  (cond((eq nil (car listaSimboloValore)) 
	(error "non sono riuscito a trovare il valore associato ad una variabile")) 
       ((equal simbolo (car (car listaSimboloValore))) 
	(second (car listaSimboloValore))) 
       ((not (equal simbolo (car (car listaSimboloValore)))) 
	(find_value simbolo (cdr listaSimboloValore))))) 

