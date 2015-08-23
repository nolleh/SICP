(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get `deriv (operator exp)) (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


;(display (deriv '(* x y) 'x))
; a. Explain what was done above. Why can't we assimilate the predicates number? 
; and samevariable? into the data-directed dispatch?
; >> because it doesn't need to be dispatched. it is common operation.

; b. Write the procedures for derivatives of sums and products, 
; and the auxiliary code required to install
; them in the table used by the program above.
; cf. ((sum? exp) (make-sum (deriv (addend exp) var)
;                           (deriv (augend exp) var)))

(define (install-sum-package)

  (define (deriv-sum exp var) 
    (make-sum (deriv (addend exp) var) 
              (deriv (augend exp) var)))
  (define (addend s) (cadr s)) ; + was gone while doing get operation
  (define (augend s) (caddr s)) ; + was gone while doing get operation
  (define (make-sum a1 a2)
    (cond ((eq? a1 0) a2)
          ((eq? a2 0) a1)
          ((and (number? a1) (number? a2))
                (+ a1 a2))
          (else (list `+ a1 a2))))
  (put `deriv `+ deriv-sum) 
  `done)

(install-sum-package)
(define exp `(+ + x 3))
(display (deriv exp `x)) ;1

;(define (test) (display `hello))
;(make-entry (list `deriv `+ test))
(display (key (car global-array)))
(display (get `deriv (operator exp))) ; deriv-sum (exp var)
(display (operands exp)) ; (+ x 3)

;(display (deriv (cadr exp) `x))

;(deriv '(* x y) 'x)
;(+ (* x 0) (* 1 y))
;(deriv '(* (* x y) (+ x 3)) 'x)
;(+ (* (* x y) (+ 1 0))
;(* (+ (* x 0) (* 1 y))
;(+ x 3)))


(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


; get / put
(define global-array `())

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