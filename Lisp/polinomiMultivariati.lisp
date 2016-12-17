; errore il caso (* 4 a) ritorna (M NUMERO (NIL))
; errore il caso (* 4 (expt 4 a)) ritorna (M NUMERO (NIL))

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

;as-variable
(defun as-variable (variables)
	(let
		(
			(var 
				(cond
					( ; CONDIZIONE1 ho una lista quindi puo essere solo un esponente tipo (expt 4 s)
						(listp (first variables))
						(list (as-exponent (car variables) ) )
					)
					( ; CONDIZIONE2 ho una variabile tipo s  # restituisce  (M TD (V 1 EXPT 4 V 1 S))
						(symbolp (car variables))
						(list (list 'v 1 (car variables) ) )
					)
					( ; CONDIZIONE3 ho un NUMERO 
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
