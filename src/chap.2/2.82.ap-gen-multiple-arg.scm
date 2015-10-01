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

(define (convert-all-args args)
	(define (inter-convert inter-args all-args)
		(if (null? inter-args) 
			#f
			(let ((args->now (arg->selected (car inter-args) args)))
				(if (not args->now)
					(arg->selected (cadr inter-args) args)
					(args->now)))))
	(inter-convert args args))

(define (arg->selected selected-arg args)
	(if (eq? (type-tag selected-arg) (type-tag (car args)))
		; same, do nothing with this.. and, just pass throught
		(cons args (convert-all-args selected-arg (cadr args)))
		; not same 2 type, and can convert it's type.
		(let ((arg->sel (get-coercion (type-tag (car args)) (type-tag selected-arg))))
			(if arg->sel
			 	; then convert, and so on to next args.
			 	(cons (arg->sel (car args)) (convert-all-args selected-arg (cadr args)))
			 	; if not to convert selected type, failed to all convert. 
			 	#f))))