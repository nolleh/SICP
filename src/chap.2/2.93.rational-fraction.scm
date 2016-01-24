; other packages
(load "C:\\Users\\Geng\\Documents\\workspace_scala\\sicp\\src\\chap.2\\2.9X.polynomials.scm")

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
      (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
           (mul (numer y) (denom x)))
           (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
          (mul (numer y) (denom x)))
          (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
          (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
          (mul (denom x) (numer y))))

  (define (=zero? x)
    (eq? (numer x) 0))

  ;(define (project x) 
  ;  (make-scheme-number (inexact->exact (floor (/ (numer x) (denom x))))))
 
  ;(define (drop x) 
  ;  (if (equ? (raise (project x)) (tag x)) (project x)
  ;      (make-rational (numer x) (denom x))))

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

(install-rational-package)

(define p1 (make-polynomial 'x 
    (list (list 2 (make-scheme-number 1)) (list 0 (make-scheme-number 1)))))

(define p2 (make-polynomial 'x 
    (list (list 3 (make-scheme-number 1)) (list 0 (make-scheme-number 1)))))
(define rf (make-rational p2 p1))
; (rational (polynomial x (3 1) (0 1)) polynomial x (2 1) (0 1))

(add rf rf)

;(rational (polynomial x (5 (scheme-number . 2)) (3 (scheme-number . 2)) (2 (scheme-number . 2)) (0 (scheme-number . 2))) polynomial x (4 (scheme-number . 1)) (2 (scheme-number . 2)) (0 (scheme-number . 1)))