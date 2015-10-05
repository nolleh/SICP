; show multiple argument. 
; all argument to 1st arg, to 2nd arg, and so on...  
; give an example this strategy is not sufficiently general. 
; (Hint: Consider the case where there are some suitable mixed-type 
;	oper- ations present in the table that will not be tried.)

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (let ((coerced-args (convert-all-args args)))
	                (if (not coerced-args)
	                	(error "No method for these types"
	                          (list op type-tags))
	                	(apply-generic op coerced-args)))))))

(define (put-coercion t1 t2 item)
  (put 'coercion (list t1 t2) item))

(define (get-coercion t1 t2)
  (get 'coercion (list t1 t2)))

;; makes all arguments as same type, which has op (if there is.)
(define (convert-all-args op args)
	;; check type set whether can be applyed.
	;; op, (list #f #f a)
	(define (check-candidate args)
		(define (all-valid? args)
			(cond ((null? args) #t)
				  ((car args) (all-valid? (cdr args)))
				  (else #f)))
		(cond ((null? args) #f)
			  ((all-valid? args)
			  (let ((type-tags (map type-tag args)))
			  	   (let ((proc (get op type-tags)))
			  	   		(if (proc #t)
			  	   			#f))))
			  (else #f))

		) ; }} check-candidate

	(define (inter-convert inter-args all-args)
		(if (null? inter-args) ; there isn't appropriate types...
			#f
			(let ((args->now (arg->selected (car inter-args) args)))
				(if (check-candidate args->now)
					(args->now); if there is, return the types list.
					(inter-convert (cdr inter-args) args)))))

	;; makes 2nd parameter to 1st's type, if possible.
	;; returns list.
	(define (arg->selected selected-arg args)
		(cond ((null? args) '())
			  ((eq? (type-tag selected-arg) (type-tag (car args)))
			  ; same, do nothing with this.. and, just pass throught
			  (cons (car args) (arg->selected selected-arg (cdr args))))
			  ; doesn't same between 2 types, and can be converted it's type.
			  (else (let ((arg->sel (get-coercion (type-tag (car args)) (type-tag selected-arg))))
				  		(if arg->sel
				 	  	; then convert, and so on to next args.
				 	  	(cons (arg->sel (car args)) (arg->selected selected-arg (cdr args)))
				 	  	; if not to convert selected type, failed to all convert. 
				 	  	#f)))))


	(inter-convert args args))

; test 
(define (some-type->scheme-number n) n)

(put-coercion 'some-type 'scheme-number some-type->scheme-number)

(put 'sum '(scheme-number scheme-number scheme-number)
     (lambda (x y z) (+ x y z)))

;(apply-generic 'sum 1 ('some-type 2 3) 


