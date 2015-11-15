; other packages
;(load "~/Documents/workspace_github/SICP/src/chap.2/2.85.drop.scm")

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
    (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
    (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
    (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
    (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
    (lambda (x y) (eq? x y)))
  (put '=zero? '(scheme-number)
    (lambda (x) (eq? x 0)))
  (put 'raise '(scheme-number)
    (lambda (x) (make-rational x 1)))
;  (put 'drop '(scheme-number) 
;    (lambda (x) (error "scheme-number doesn't have lower type")))
  (put 'negate '(scheme-number)
    (lambda (x) (tag (* -1 x))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
           (* (numer y) (denom x)))
           (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
          (* (numer y) (denom x)))
          (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
          (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
          (* (denom x) (numer y))))

  (define (=zero? x)
    (eq? (numer x) 0))

  (define (project x) 
    (make-scheme-number (inexact->exact (floor (/ (numer x) (denom x))))))
 
  (define (drop x) 
    (if (equ? (raise (project x)) (tag x)) (project x)
        (make-rational (numer x) (denom x))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
    (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
    (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
    (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
    (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
    (lambda (x y) (= (* (numer x) (denom y)) (* (numer y) (denom y)))))
  (put '=zero? '(rational)
    (lambda (x) (=zero? x)))
  (put 'raise '(rational)
    (lambda (x) (make-complex-from-real-imag (inexact->exact (floor (/ (numer x) (denom x)))) 0)))
  (put 'drop '(rational) drop)

  (put 'negate '(rational)
    (lambda (x) (tag (make-rat (* -1 (numer x)) (denom x)))))
  
  (put 'make 'rational
    (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (define (equ? z1 z2)
    (and (eq? (real-part z1) (real-part z2)) (eq? (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (and (eq? (real-part z) 0) (eq? (imag-part z) 0)))

  ;;;; -----------exercise 85.-----------------------
  (define (project z)
    (make-rational (real-part z) 1))
  (define (drop z)
    (if (equ? (raise (project z)) (tag z)) (project z)
        (make-complex-from-real-imag (real-part z) (imag-part z))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ? z1 z2)))
  (put '=zero? '(complex)
       (lambda (z) (=zero? z)))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
 
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'raise '(complex)
    (lambda (z) (error "complex doesn't have super type")))
  (put 'drop '(complex) drop)
  (put 'negate '(complex)
    (lambda (z) (tag (make-from-real-imag 
      (* -1 (real-part z)) (* -1 (imag-part z))))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
'done)

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    ;(error "Bad tagged datum: TYPE-TAG" datum)))
    #f))
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)))

(define (apply-generic op . args)
   (let ((type-tags (map type-tag args)))
     (let ((proc (get op type-tags)))
       (if proc
          ;(apply proc (map contents args))
          (let ((apl (apply proc (map contents args))))
              (if (get 'drop (list (type-tag apl))) 
                ((get 'drop (list (type-tag apl))) (contents apl))
                ;(drop apl)
                apl))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2) 
                  (error "No method for thease types" (list op type-tags))
                  (let ((t1-level (level type1 tower 0))
                        (t2-level (level type2 tower 0)))
                        (let ((diff (- t2-level t1-level)))
                              (if (> diff 0)
                                  (apply-generic op (raise-until a1 diff) a2)
                                  (apply-generic op a1 (raise-until a2 (* -1 diff))))))))
              (error "No method for these types"
                    (list op type-tags)))))))


; get / put
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list (make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))


(define (add x y)     (apply-generic 'add x y))
(define (sub x y)     (apply-generic 'sub x y))
(define (mul x y)     (apply-generic 'mul x y))
(define (div x y)     (apply-generic 'div x y))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z)     (apply-generic 'angle z))
(define (equ? x y)    (apply-generic 'equ? x y))
(define (=zero? x)    (apply-generic '=zero? x))
(define (raise x)     
  (let ((proc (get 'raise (list (type-tag x)))))
    (if proc (proc (contents x)))))

(define (drop x)      (apply-generic 'drop x))

; super / tower ---> higher index
(define tower (list 'scheme-number 'rational 'complex))
(define (level type higherachy index)
  (cond ((null? higherachy) #f)
        ((eq? (car higherachy) type) index)
        (else (level type (cdr higherachy) (+ index 1)))))

(define (raise-until a times)
  (if (<= times 0) a
      (raise-until (raise a) (- times 1))))


(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)


(define (install-polynomial-package)
	;; internal procedures
	;; representation of poly
	(define (make-poly variable term-list)
		(cons variable term-list))
	(define (variable p) (car p))
	(define (term-list p) (cdr p))

	(define (variable? x) (symbol? x))
	(define (same-variable? v1 v2)
  		(and (variable? v1) (variable? v2) (eq? v1 v2)))

	;; representation of terms and termlists	
    (define (adjoin-term term term-list)
      (if (=zero? (coeff term))
          term-list
          (cons term term-list)))
    (define (the-empty-termlist) '())
    (define (first-term term-list) (car term-list))
    (define (rest-terms term-list) (cdr term-list))
    (define (empty-termlist? term-list) (null? term-list))
    (define (make-term order coeff) (list order coeff))
    (define (order term) (car term))
    (define (coeff term) (cadr term))

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
	(put 'make '(polynomial)
		(lambda (var terms)
			(tag (make-poly var terms))))
	(put '=zero? '(polynomial)
		(lambda (p) 
			(empty-termlist? (term-list p))))
	'done)

(define (make-polynomial var terms)
    ((get 'make '(polynomial)) var terms))
(define (negate x)     (apply-generic 'negate x))

(install-polynomial-package)

(define poly (make-polynomial 'x (list (list 2 (make-scheme-number 2)))))
(define poly2 (make-polynomial 'x (list (list 2 (make-scheme-number 3)))))

(negate poly) ;(polynomial x (2 (scheme-number . -2)))

(sub poly poly2) ;(polynomial x (2 (scheme-number . -1)))
