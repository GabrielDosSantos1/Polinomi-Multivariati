;as-monomial ATTENZIONE prende in input una espressione WARNING ci sono degli warning 
(defun as-monomial (expression) 
	(let 
		(
			(coefficient
				(find-coefficient 1 expression 1)
			)
			(vars-n-powers  ; bisogna ordinarlo
				(as-expression expression)
			)
			(total-degree
				(find-total-degree 0 vars-n-powers)
			)
		)
		(declare (special vars-n-powers))
		(list 'm coefficient total-degree vars-n-powers)
	)
)

;as-expression ATTENZIONE prende in input una expressione e chiama ricorsivamente as-variable
(defun as-expression (expression)
	(cond
		(
			(and
				(numberp (car expression))
				(eq nil (cdr expression))
			)
			nil
		)
		(
			(and
				(symbolp (car expression))
				(eq nil (cdr expression))
				(not(eq ' * (car expression)))
			)
			(as-variable expression)
		)
		(
			( eq '* (car expression) )
			( as-variable (cdr expression) )
		)
		(
			( eq 'expt (car expression) )
			(as-variable (list expression) )
		)
	)
)

;find-coefficient INPUT una expression OUTPUT la moltiplicazione dei coefficienti
(defun find-coefficient (n monomial count)
	(let
		(
			(coefficient
				(cond
					( ; CONDIZIONE1 siamo nel caso (* 1 ....)
						(and
							(numberp (car monomial) )
							(or (= count 1) (> 0 (car monomial) ) )
						)
						( * n (car monomial) )
					)
					( ; CONDIZIONE2 siamo nel caso (* ... (4)  ...)  ALT s*-4*s non è un monomio
						(and 									    ;	 s*(-4)*x è un monomio	
							(listp (car monomial) )
							(numberp (car (car monomial)))
						)
						(* n (car (car monomial)) )
					)
					( ; CONDIZIONE3 siamo nel caso non è un numero
						(and
							(not (listp (car monomial)))
							(not (numberp ( car (car monomial) )))
						) ; non è un numero 
						n
					)					
				)
			)
		)
		(cond ; # trasformare in if
			( ; CONDIZIONE1 non ci sono piu elementi da analizzare
				(eq nil (cdr monomial) )
				coefficient
			)
			( ; CONDIZIONE2 bisogna fare una chiamata ricorsiva
				T
				(find-coefficient coefficient ( cdr monomial ) (+ 1 count) )  
			)
		)
	)
)

;find-total-degree INPUT un monomio gia parsato senza numeri
(defun find-total-degree (sum monomial)
	(let
		(
			(var-sum
				(cond ; dobbiamo trovare tutti i (v n s) "le variabili"
					(
						(and
							(listp (car monomial))
							(eq 'v (first (car monomial)) )
							(< 0 (second (car monomial)) )
							(symbol (third (car monomial)) )
						) 
						(+ sum (second (car monomial)) )
					)
				)		
			)
		)
		(cond ; # trasformare in if
			( ; CONDIZIONE1 non ci sono piu elementi da analizzare
				(eq nil (cdr monomial) )
				var-sum
			)
			( ; CONDIZIONE2 bisogna fare una chiamata ricorsiva
				T
				(find-total-degree var-sum ( cdr monomial ) )  
			)
		)
	)
)

;as-variable ATTENZIONE prende in input liste del tipo (4 s (expt 4 s)) fa la chiamata ricorsiva in caso di più elementi
(defun as-variable (variables)
	(let
		(
			(var 
				(cond
					( ; CONDIZIONE1 ho una lista quindi puo essere solo un esponente tipo (expt 4 s) 	# funziona
						(listp (first variables))
						(list (as-exponent (car variables) ) )
					)
					( ; CONDIZIONE2 ho una variabile tipo s  											# funziona
						(symbolp (car variables))
						(list (list 'v 1 (car variables) ) )
					)
					( ; CONDIZIONE3 ho un NUMERO 														# funziona
						(numberp (car variables))
						'()
					)
				)
			)
		)
		(cond
			( ; CONDIZIONE1 non ci sono piu elementi da analizzare
				(eq nil (cdr variables) )
				var
			)
			( ; CONDIZIONE2 bisogna fare una chiamata ricorsiva
				T
				(append var (as-variable (cdr variables) ) )  ; ####### ritorna cosi perche ?(V (1 S) (V (4 S)))
			)
		)
	)
)

;as-exponent ATTENZIONE prende in input liste del tipo (exp 4 s)
(defun as-exponent (exponent)
	(cond
		( ; CONDIZIONE1 è della forma (expt 5 a) 		# funziona
			(and
				(eq 'expt (first exponent) )
				(numberp (second exponent) )
				(< 0 (second exponent))
				(symbolp (third exponent) )
			)
			(list 'v (second exponent) (third exponent) )
		)
		( ; CONDIZIONE2 input è nella forma (expt 0 s)	# funziona
			(and
				(eq 'expt (first exponent)) 
	      		(numberp (second exponent))
	      		(= 0 (second exponent))
				(symbolp (third exponent))
			) 
			'()
		)
	)
)

;(first (car (car '((V 3 S) (V 5 D)))))
; print-variable INPUT sara di questo tipo ((V 3 S) (V 5 D) ...)
(defun print-variable (variables)
	(let
		(
			(var 
				(cond
					(
						(and
							(listp variables)
							(listp (car variables))
							(eq 'v (first (car variables)))
							(numberp (second (car variables)))
							(symbolp (third (car variables)))
						)
						(cond 
							(
								(= 1 (second variables)) 
								(list (third variables))
							)
							(
								(> 1 (second variables))
								(list (third variables) '^ (second variables))
							)
						)
					)
				)
			)
		)
		(cond
			( ; CONDIZIONE1 non ci sono piu elementi da analizzare
				(eq nil (cdr variables) )
				var
			)
			( ; CONDIZIONE2 bisogna fare una chiamata ricorsiva
				T
				(append
					(append var (list '*))
					(print-variables (cdr variables))
				) ; qui in mezzo dovrebbe esserci il *
			)
		)
	)
)

; INPUT sara di questo tipo (M 1 9 ((V 3 S) (V 3 T) (V 1 W) (V 1 X) (V 1 Y)))
(defun print-monomial (monomial)
	(cond
		(
			(eq 'm (car monomial))
			(let
				(
					(coefficient 
						(second monomial)
					)
					(variable
						(print-variable (fourth monomial))
					)
				)
				(cond
					( (= coefficient 1) (list '+ variable) )
					( (= coefficient -1) (list '- variable) )
					(T (list coefficient variable) )
				)
			)
		)
	)
)

(defun pprint-polynomial (expression)
	(let
		(
			(monomial
				(cond
					(
						(and
							(listp expression)
							(eq 'p (car expression))
							(listp (second expression))
						) 
						(print-monomial (car (second expression) ) )
					)
				)
			)
			(restList
				(cond
					(
						(listp expression)
						(listp (cdr expression) )
						(listp (car (cdr expression) ) )
						( cdr (car (cdr expression) ) ) 
					)
				)
			)
		)
		(cond
			( ; CONDIZIONE1 non ci sono piu elementi da analizzare
				(eq nil restList )
				monomial
			)
			( ; CONDIZIONE2 bisogna fare una chiamata ricorsiva
				T
				(append monomial (print-polynomial (list 'p restList ) ) ) 
			)
		)
	)
)