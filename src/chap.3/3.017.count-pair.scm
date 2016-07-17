(define (count-pairs x)
  (let ((pairs '()))
    (define (inner-pairs x)
      (if (not (pair? x))
        0
        (let ((inc (element-of-set? (car x) pairs)))
          (if (eq? inc 1)
            (set! pairs (cons (car x) pairs)))
          (+ (inner-pairs (car x))
             (inner-pairs (cdr x))
             inc))))
  (inner-pairs x)))

(define (element-of-set? x set)
  (cond ((null? set) 1)
        ((eq? x (car set)) 0)
        (else
          (element-of-set? x (cdr set)))))

(count-pairs '(a b))
(count-pairs '(a a b))
(count-pairs '(a b a))
(count-pairs '(a b b a c))