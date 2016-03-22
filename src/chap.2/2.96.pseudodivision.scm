; define dense polynomials .. 
; other packages
;(load "C:\\Users\\Geng\\Documents\\workspace_scala\\sicp\\src\\chap.2\\2.8X.packages.scm")
(load "~/Documents/workspace_github/sicp/src/chap.2/2.8X.packages.scm")

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
      (and (variable? v1) (variable? v2) (eq? v1 v2)))

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

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (let ((div-res (div-terms (term-list p1) (term-list p2))))
        (list (tag (make-poly (variable p1) (car div-res)))
              (cadr div-res)))
      (error "Polys not in same var: DIV-POLY"
      (list p1 p2))))

  (define (add-terms L1 L2)
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
                      (make-term  (order t1)
                                  (add (coeff t1) (coeff t2)))
                      (add-terms  (rest-terms L1)
                                  (rest-terms L2)))))))))
  (define (sub-terms L1 L2)
      (cond ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (else
             (let ((t1 (first-term L1))
                   (t2 (first-term L2)))
               (cond ((> (order t1) (order t2))
                        (adjoin-term
                          t1 (sub-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                        (adjoin-term
                          t2 (sub-terms L1 (rest-terms L2))))
                     (else
                       (adjoin-term
                       (make-term (order t1)
                                  (sub (coeff t1) (coeff t2)))
                       (sub-terms (rest-terms L1)
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

  (define (div-terms L1 L2) 
    (if (empty-termlist? L1) 
      (list (the-empty-termlist) (the-empty-termlist)) 
      (let ((t1 (first-term L1)) (t2 (first-term L2)))
        (if (> (order t2) (order t1)) 
          (list (the-empty-termlist) L1) 
          (let ((new-c (div (coeff t1) (coeff t2))) 
            (new-o (- (order t1) (order t2)))) 
            (let ((rest-of-result (sub-terms L1 
              (mul-terms (list (make-term new-o new-c)) L2)) ))
              ;; 몫 텀 + (나머지 텀 / 나눔수 텀리스트)
              (list (add-terms (list (make-term new-o new-c)) 
                (car (div-terms rest-of-result L2))) 
                (cadr (div-terms rest-of-result L2)))))))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
          (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: GCD-POLY"
      (list p1 p2))))

  (define (gcd-terms a b)
    (if (empty-termlist? b) 
      (car (div-terms a 
        (list (make-term 0 (apply gcd (map coeff (term-list a))))))) 
      (gcd-terms b (pseudo-remainder-terms a b)))) 

  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  (define (pseudo-remainder-terms a b)
    (let ((integerizing-factor
      ;c^(1+O1−O2)
      (list (make-term 0 (expt (coeff (first-term b)) 
              (+ 1 (- (order (first-term a)) (order (first-term b))))) )) ))
      (cadr (div-terms (mul-terms a integerizing-factor) b))))

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
  (put 'div '(polynomial polynomial)
    (lambda (p1 p2) (div-poly p1 p2)))
  (put 'make '(polynomial)
    (lambda (var terms)
      (tag (cons var terms))))

  (put '=zero? '(polynomial)
    (lambda (p) 
      (empty-termlist? (term-list p))))

  (put 'variable '(polynomial) variable)
  (put 'term-list '(polynomial) term-list)
  (put 'gcd '(polynomial polynomial) 
    (lambda (p1 p2) (tag (gcd-poly p1 p2))))

  'done-polynomial)

(define (make-poly var terms)
  ((get 'make '(polynomial)) var terms))

(define (greatest-common-divisor p1 p2)
  (apply-generic 'gcd p1 p2))

(define (negate x) (apply-generic 'negate x))
(define (variable p) (apply-generic 'variable p))
(define (term-list p) (apply-generic 'term-list p))

(install-polynomial-package)

(define p1 (make-poly 'x '((2 1) (1 2) (0 1))))
(define p2 (make-poly 'x '((2 11) (0 7))))
(define p3 (make-poly 'x '((1 13) (0 5))))

(define q1 (mul p1 p2))
; (polynomial x (4 11) (3 22) (2 18) (1 14) (0 7))
(define q2 (mul p1 p3))
; (polynomial x (3 13) (2 31) (1 23) (0 5))
(greatest-common-divisor q1 q2)
;1: (polynomial x (2 1458) (1 2916) (0 1458))
;2: (polynomial x (2 1) (1 2) (0 1))