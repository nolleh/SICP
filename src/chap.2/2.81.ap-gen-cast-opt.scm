; exercise 2.81
; a. What happens if we call exp with two complex numbers as ar- guments?
; >> 1. call apply-generic with 'complex 'complex 
; >> 2. get proc -> fail
; >> 3. coerce arg 1 (complex -> complex) / coerce arg 2 (complex->com..)
; >> 4. coerce arg 1 exists, so called apply-generic with arg1's coerce, which is actually same !! 
; >>> 4's call is actually same with 1's function call..(result in infinite loop.)

; b. a's solution is well defined ?
; >> NO,, as like saw in upper example.

; c. Modify apply-generic so that it doesn’t try coercion 
; 	 if the two arguments have the same type.
;

(define (apply-generic op . args)
   (let ((type-tags (map type-tag args)))
     (let ((proc (get op type-tags)))
       (if proc
           (apply proc (map contents args))
           (if (= (length args) 2)
               (let ((type1 (car type-tags))
                     (type2 (cadr type-tags))
                     (a1 (car args))
                     (a2 (cadr args)))
               	 (if (eq? type1 type2) 
               	 	 (error "No method for thease types" (list op type-tags))
	                 (let ((t1->t2 (get-coercion type1 type2))
	                       (t2->t1 (get-coercion type2 type1)))
	                   (cond (t1->t2
	                          (apply-generic op (t1->t2 a1) a2))
	                         (t2->t1
	                          (apply-generic op a1 (t2->t1 a2)))
	                         (else (error "No method for these types"
	                                      (list op type-tags)))))))
               (error "No method for these types"
                      (list op type-tags)))))))