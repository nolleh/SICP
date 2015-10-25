; design
; a procedure that raises objects of that type one level in the tower.

(define (add x y)    (apply-generic 'add x y))
(define (sub x y)    (apply-generic 'sub x y))
(define (mul x y)    (apply-generic 'mul x y))
(define (div x y)    (apply-generic 'div x y))
(define (raise x)    (apply-generic 'raise x))

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
  (put 'raise '(scheme-number)
    (lambda (x) (make-rational x 1)))
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
  (put 'raise '(rational)
    (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'make 'rational
    (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
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

  (define (real-part z)
      (apply-generic 'real-part z))
  (define (imag-part z)
      (apply-generic 'imag-part z))
  (define (magnitude z)
      (apply-generic 'magnitude z))
  (define (angle z)
      (apply-generic 'angle z))

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
  (put 'real-part 'complex 
    (lambda (z) (real-part z)))
  (put 'imag-part 'complex imag-part)
  (put 'magnitude 'complex magnitude)
  (put 'angle 'complex angle)

  (put 'raise '(complex)
    (lambda (z) (error "complex doesn't have super type")))
  (put 'make-from-real-imag 'complex
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
    (lambda (r a) (tag (make-from-mag-ang r a))))
'complex-done)

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
    (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)))

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
                   (let ((t1-level (level type1 tower 0))
                         (t2-level (level type2 tower 0)))
                         (let ((diff (- t2-level t1-level)))
                               (if (> diff 0)
                                   (apply-generic op (raise-until a1 diff) a2)
                                   (apply-generic op a1 (raise-until a2 (* -1 diff))))))))
               (error "No method for these types"
                      (list op type-tags)))))))

; super / tower ---> higher index
(define tower (list 'scheme-number 'rational 'complex))
(define (level type higherachy index)
  (cond ((null? higherachy) #f)
        ((eq? (car higherachy) type) index)
        (else (level type (cdr higherachy) (+ index 1)))))

(define (raise-until a times)
  (if (<= times 0) a
      (raise-until (raise a) (- times 1))))


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


(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)

(raise (make-scheme-number 3)) ; (rational 3 . 1)

(add (make-scheme-number 3) (make-rational 3 1)) ; (rational 6 . 1)

(add (make-scheme-number 3) (make-complex-from-real-imag 2 1))