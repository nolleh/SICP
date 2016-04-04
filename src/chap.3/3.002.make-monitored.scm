(define (make-monitored f)
	(let ((count 0))
		(define (dispatch msg)
			(cond ((eq? msg 'how-many-calls?)
				  	count)
				  ((eq? msg 'reset-count)
					(set! count 0))
				  (else 
				  	(begin (set! count (+ count 1))
				  		(f msg)))))
	dispatch))
(define s (make-monitored sqrt))

(s 100) ; 10
(s 'how-many-calls?) ; 1

(s 25) ; 5
(s 'how-many-calls?) ; 2