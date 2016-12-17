;as-monomial
(defun as-monomial (expression)
	(cond 	(	;CONDIZIONE1 caso in cui '(4) 			#funziona
				(and	
					(numberp (car expression) )
					(eq nil (cdr expression) )
				)
				(list 'm (car expression) 0 nil )
			)
			(	;CONDIZIONE2 caso '(s) 					#funziona
				(and	
					(symbolp (car expression) )
					(eq nil (cdr expression) )
					(not(eq '* (car expression)))
				)
				( list 'm 1 (as-variable expression ) )
			)
			(	;CONDIZIONE3 caso '(* ....)				# manca il TD
				(
					eq '* (car expression) 
				)
				(list 'm 'TD ( as-variable (cdr expression) ) )
			)
			(	;CONDIZIONE4 caso '(expt 4 s)  			# manca il TD
				(
					eq 'expt (car expression)
				)
				(list 'm 'TD (as-variable  (list expression) ) )
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

;as-variable
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
						(list (car variables ) )
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

;as-exponent
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
			(list 1)
		)
	)
)