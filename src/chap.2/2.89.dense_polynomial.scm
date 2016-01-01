; define dense polynomials .. 
; other packages
(load "C:\\Users\\Geng\\Documents\\workspace_scala\\sicp\\src\\chap.2\\2.8X.packages.scm")

(define (install-polynomial-package)
	;; internal procedures
	;; representation of poly
	(define (make-sparse-poly variable term-list)
		((get 'make-poly '(sparse-poly)) 
			variable term-list))

	(define (make-dense-poly variable term-list)
		((get 'make-poly '(dense-poly)) 
			variable term-list))

	(define (variable? x) (symbol? x))
	(define (same-variable? v1 v2)
  		(and (variable? v1) (variable? v2) (eq? v1 v2)))

    (define (the-empty-termlist) '())
    (define (first-term term-list) (car term-list))
    (define (rest-terms term-list) (cdr term-list))
    (define (empty-termlist? term-list) (null? term-list))

    ; Doesn't know what represent type to be chosen.
    ; default : sparse. 
    ; (similary, the complex package 
	; also does not have strict policy)
    (define (make-poly variable term-list)
    	(make-sparse-poly variable term-list))
    (define (adjoin-term term term-list)
		(sparse-adjoin-term term term-list))

	(define (add-poly p1 p2)
		;(display (list p1 p2))
		(if (same-variable? (variable p1) (variable p2))
				(make-poly (variable p1)
					(add-terms (term-list p1)
						(term-list p2)))
			(error "Polys not in same var: ADD-POLY"
			(list p1 p2))))
	(define (mul-poly p1 p2)
		(if (same-variable? (variable p1) (variable p2))
				(make-poly (variable p1)
					(mul-terms (term-list p1)
						(term-list p2)))
			(error "Polys not in same var: MUL-POLY"
			(list p1 p2))))

	(define (add-terms L1 L2)
      ;(display (list L1 L2))
      (cond ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (else
             (let ((t1 (first-term L1))
                   (t2 (first-term L2)))
               (cond ((> (order t1) (order t2))
                        (adjoin-term
                       		t1 (add-terms (rest-terms L1) L2)))
               		 ((< (order t1) (order t2))
                        (adjoin-term
                       		t2 (add-terms L1 (rest-terms L2))))
                     (else
                       (adjoin-term
                       (make-term (order t1)
                                  (add (coeff t1) (coeff t2)))
                       (add-terms (rest-terms L1)
                                  (rest-terms L2)))))))))

	(define (mul-terms L1 L2)
      (if (empty-termlist? L1)
          (the-empty-termlist)
          (add-terms (mul-term-by-all-terms (first-term L1) L2)
                     (mul-terms (rest-terms L1) L2))))
	(define (mul-term-by-all-terms t1 L)
    	(if (empty-termlist? L)
        	(the-empty-termlist)
        (let ((t2 (first-term L)))
        	(adjoin-term
            	(make-term (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (negate p)
  	(make-poly (variable p)
    	(mul-term-by-all-terms (make-term 0 
        	(make-scheme-number -1)) (term-list p))))

	;; interface to rest of the system
	(define (tag p) (attach-tag 'polynomial p))
	(put 'add '(polynomial polynomial)
		(lambda (p1 p2) (tag (add-poly p1 p2))))
	(put 'negate '(polynomial)
		(lambda (p) (tag (negate p))))
	(put 'sub '(polynomial polynomial)
	    (lambda (p1 p2) 
	    	(tag (add-poly p1 (negate p2)))))
	(put 'mul '(polynomial polynomial)
		(lambda (p1 p2) (tag (mul-poly p1 p2))))
	(put 'make-sparse '(polynomial)
		(lambda (var terms)
			(tag (make-sparse-poly var terms))))
	(put 'make-dense '(polynomial)
		(lambda (var terms)
			(tag (make-dense-poly var terms))))
	(put '=zero? '(polynomial)
		(lambda (p) 
			(empty-termlist? (term-list p))))

	(put 'variable '(polynomial) variable)
	(put 'term-list '(polynomial) term-list)
	;(put 'adjoin-term '(polynomial) 
	;	(lambda (t p) (display "polynomial-adjoin-term")
 ;(adjoin-term t p)))

	'polynomial-done)

(define (make-sparse-poly var terms)
	((get 'make-sparse '(polynomial)) var terms))
(define (make-dense-poly var terms)
	((get 'make-dense '(polynomial)) var terms))


(define (negate x)     (apply-generic 'negate x))
(define (adjoin-term term term-list) 
	(apply-generic 'adjoin-term term term-list))

;; done polynomial
    
(define (install-sparse-polynomial-package)
	; individual term must know it's order,
	; no matter what polynomial type.
	;(define (make-term order coeff) 
    ; 	(list order coeff))

	(define (tag z) (attach-tag 'sparse-poly z))
	; for tagging
	(define (make-sparse-poly variable term-list)
		(tag (cons variable term-list)))

	; variable and term list is appear in unwrapped polynomial.
	(define (variable p) (car p))
	(define (term-list p) (cdr p))

	; it only differ from dense, by making term-list
	; in dense, it add without order....
	(define (adjoin-term term term-list)
      	(if (=zero? (coeff term))
        	term-list
        	(cons term term-list)))

    (put 'make-poly '(sparse-poly) make-sparse-poly)
	(put 'variable '(sparse-poly) variable)
	(put 'term-list '(sparse-poly) term-list)
    (put 'adjoin-term '(sparse-poly) adjoin-term)

	'done-sparse-polynomial)

(define (install-dense-polynomial-package)


	(define (tag z) (attach-tag 'dense-poly z))
	; for tagging
	(define (make-dense-poly variable term-list)
		(tag (cons variable term-list)))

	; variable and term list is appear in unwrapped polynomial.
	(define (variable p) (car p))
	(define (term-list p) (cdr p))

	; it only differ from dense, by making term-list
	; in dense, it add without order....
	(define (adjoin-term term term-list)
      	(if (=zero? (coeff term))
        	term-list
        	(cons (coeff term) term-list)))

    (put 'make-poly '(dense-poly) make-dense-poly)
	(put 'variable '(dense-poly) variable)
	(put 'term-list '(dense-poly) term-list)
    (put 'adjoin-term '(dense-poly) adjoin-term)
	'done-dense-polynomial)


(define (variable p) (apply-generic 'variable p))
(define (term-list p) (apply-generic 'term-list p))
;(define (make-sparse-term order coeff)
;	((get 'make-term '(sparse-poly)) order coeff))
;(define (make-dense-term order coeff)
;	((get 'make-term '(dense-poly)) order coeff))

(define (sparse-adjoin-term t l)
	((get 'adjoin-term '(sparse-poly)) t l))
(define (dense-adjoin-term t l)
	((get 'adjoin-term '(dense-poly)) t l))

;(define (adjoin-term term p)
;	(display "global adjoin-term")
;	(display p)
;	((get 'adjoin-term (type-tag p)) term (term-list p)))
;; done sparse-polynomial

(install-polynomial-package)
(install-sparse-polynomial-package)
(install-dense-polynomial-package)

(define poly (make-sparse-poly 'x 
	(list (list 2 (make-scheme-number 2)))))
(display poly) ;(polynomial sparse-poly x (2 (scheme-number . 2)))


(define poly2 (make-dense-poly 'x 
	(list (list 2 (make-scheme-number 3)))))
(display poly2) ;(polynomial dense-poly x (scheme-number . 3))

(add poly poly2)
