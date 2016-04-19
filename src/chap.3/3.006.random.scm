(define (rand-update x)
  (let ((m (expt 2 32))
        (a 1664525)
        (b 1013904423))
  (remainder (+ (* a x) b) m)))

(define (random-generator init-value)
  (define x init-value)
  (define (dispatch m)
    (cond ((eq? m 'generate)
          (set! x (rand-update x)) x)
          ((eq? m 'reset)
          (set! x init-value))))
dispatch)

(define rg (random-generator 10))
(rg 'generate) ; 1030549673
(rg 'generate) ; 1130070716

(rg 'reset) ; 1130070716
(rg 'generate) ;1030549673
(rg 'generate) ; 1130070716