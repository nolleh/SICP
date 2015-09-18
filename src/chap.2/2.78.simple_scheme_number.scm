(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (define proc 
      (if (car type-tags) ; (get 'add (list #f #f))
      (get op type-tags)
      (get op)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types -- APPLY-GENERIC"
          (list op type-tags)))))
; (apply (get 'add) (map contents (list (make-scheme-number 3) (make-scheme-number 4))))
;Value: 7
; change type-tag, contents, and attach-tag
; and use scheme-number without tag
;; --------------- exercise 2.77. ------------
(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum)
        (car datum))
        ((number? datum) #f)
        (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (cond (pair? datum)
        (cdr datum)
        (number? datum) (datum)
        (error "Bad tagged datum -- CONTENTS" datum)))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add
       (lambda (x y) (tag (+ x y))))
  (put 'sub
       (lambda (x y) (tag (- x y))))
  (put 'mul
       (lambda (x y) (tag (* x y))))
  (put 'div
       (lambda (x y) (tag (/ x y))))
  (put 'make
       (lambda (x) (tag x)))
  'done)

; get / put
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

; no overloading....
;(define (put op type item)
;  (define (put-helper k array)
;    (cond ((null? array) (list (make-entry k item)))
;          ((equal? (key (car array)) k) array)
;          (else (cons (car array) (put-helper k (cdr array))))))
;  (set! global-array (put-helper (list op type) global-array)))

;(define (get op type)
;  (define (get-helper k array)
;    (cond ((null? array) #f)
;          ((equal? (key (car array)) k) (value (car array)))
;          (else (get-helper k (cdr array)))))
;  (get-helper (list op type) global-array))

(define (put op item)
  (define (put-helper k array)
    (cond ((null? array) (list (make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op) global-array)))

(define (get op)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op) global-array))



;; ----------- exercise 2.77. -------------
(define (make-scheme-number x)
  ((get 'make) x))


(install-scheme-number-package)
(display (add (make-scheme-number 3) (make-scheme-number 4))) ; => 7