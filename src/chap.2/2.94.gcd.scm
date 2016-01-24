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
  ; Doesn't know what represent type to be chosen.
  ; default : sparse. 
  ; (similary, the complex package 
  ; also does not have strict policy)
  (define (make-poly variable term-list)
    (make-sparse-poly variable term-list))

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
        (list (make-poly (variable p1)
          (car (div-terms (term-list p1)
            (term-list p2)))) 
          (cadr (div-terms (term-list p1) (term-list p2))))))

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
  (define (gcd-terms a b) 
    (if (empty-termlist? b) 
      a 
      (gcd-terms b (remainder-terms a b)))) 

  (define (negate p)
    (make-poly (variable p)
      (mul-term-by-all-terms (make-term 0 
          (make-scheme-number -1)) (term-list p))))

  (define (flatmap f lst)
    (apply append (map f lst)))

  (define (find predicate list)
    (if (null? list) #f
    (if (predicate (car list)) (car list)
        (find predicate (cdr list)))))

  (define var-tower '(x y z))
  ; ing -> sudo code (only idea)
  ; 1. 대상 poly 들의 변수들 - tower 순으로 추출 - x,z / z
  (define (extract-variables p)
    (map (lambda (sel-var) 
      (find (lambda (var) (if (eq? sel-var var) #t #f) )
        (map (lambda (t) 
          (variable (coeff t))) (term-list p)))
     ) var-tower))

  ; 2. 두 poly 변수들에서 겹치는 변수를 찾음 - z
  (define (matched-variable vl1 vl2)
    (car (filter (lambda (vl) (not eq? '0))
      (map (lambda (v1) (map (lambda (v2) (
        (if (eq? v1 v2) (v1)
            ('0)))) v2)) v1))))

  ; 3. 모체 다항식의 variable 을 mul 하여 새로운 텀리스트를 얻고, 
  ; 4. 새 텀리스트로부터 해당하는 변수의 다항식을 coeff 로 가진 term 들을 찾는다.
  ; 5. 

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
    (lambda (p1 p2) (tag (div-poly p1 p2))))
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

  (put 'extract-variables '(polynomial) extract-variables)
  'polynomial-done)

(define (make-sparse-poly var terms)
  ((get 'make-sparse '(polynomial)) var terms))
(define (make-dense-poly var terms)
  ((get 'make-dense '(polynomial)) var terms))

(define (test p)
  ((get 'extract-variables '(polynomial)) p))

(define (negate x)     (apply-generic 'negate x))

;; done polynomial
    
(define (install-sparse-polynomial-package)
  ; individual term must know it's order,
  ; no matter what polynomial type.
  ;(define (make-term order coeff) 
    ;   (list order coeff))

  (define (tag z) (attach-tag 'sparse-poly z))
  ; for tagging
  (define (make-sparse-poly variable term-list)
    (tag (cons variable term-list)))

  ; variable and term list is appear in unwrapped polynomial.
  (define (variable p) (car p))
  (define (term-list p) (cdr p))


  (put 'make-poly '(sparse-poly) make-sparse-poly)
  (put 'variable '(sparse-poly) variable)
  (put 'term-list '(sparse-poly) term-list)

  'done-sparse-polynomial)

(define (install-dense-polynomial-package)

  (define (tag z) (attach-tag 'dense-poly z))

  ;; 좋은 방법은 아닌것 같지만...
  ;; add-poly 등도 적용이 되려면 term-list selector 에 대해 일관적인
  ;; 리턴이 되어야 한다.. (그렇지 않으면 섞여있을때 어떻게 더해야할지?)
  ; ((0 1) (3 2)) -> (1 0 0 2)
  ; concatmap 으로도 할 수 있을듯..
  (define (convert-dense-form sparse-terms)
    ; 지금 넣을 인덱스 까지 비어 있는 인덱스를 채움
    (define (iter-form dense-i terms)
      (if (null? terms) 
        ()
        (let ((t (car terms)))
          (if (eq? (car t) dense-i)
            (cons (cadr t) (iter-form (+ dense-i 1) (cdr terms)))
          (cons 0 (iter-form (+ dense-i 1) terms))))))
    (iter-form 0 sparse-terms))
      
  ; (1 0 0 2) -> ((0 1) (3 2))
  (define (convert-sparse-form dense-terms)
    (define (iter-form dense-i terms)
      (if (null? terms) ()
        (if (eq? 0 (car terms))
          (iter-form (+ dense-i 1) (cdr terms))
          (cons (cons dense-i (list (car terms)))
            (iter-form (+ dense-i 1) (cdr terms))))))
    (iter-form 0 dense-terms))
    

  ; for tagging
  (define (make-dense-poly variable term-list)
    (tag (cons variable (convert-dense-form term-list))))

  ; variable and term list is appear in unwrapped polynomial.
  (define (variable p) (car p))
  (define (term-list p) 
    (convert-sparse-form (cdr p)))

  (put 'make-poly '(dense-poly) make-dense-poly)
  (put 'variable '(dense-poly) variable)
  (put 'term-list '(dense-poly) term-list)
  'done-dense-polynomial)


(define (variable p) (apply-generic 'variable p))
(define (term-list p) (apply-generic 'term-list p))

(install-polynomial-package)
(install-sparse-polynomial-package)
(install-dense-polynomial-package)

(define poly (make-sparse-poly 'z 
  (list (list 1 (make-scheme-number 1)))))

(define poly2 (make-sparse-poly 'y 
  (list (list 1 (make-scheme-number 1)))))

(define complex-poly (make-sparse-poly 'x 
  (list (list 1 poly) (list 0 poly2))))

(display (test complex-poly))