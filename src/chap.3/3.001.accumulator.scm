(define (make-accumulator n)
	(lambda (a) (begin (set! n (+ n a))
		n)))


(define A (make-accumulator 5))
(A 10)
(A 10)
(A 25)