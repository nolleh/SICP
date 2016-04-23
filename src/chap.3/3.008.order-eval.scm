
(define some-v -1)
(define (f n)
  (if (eq? some-v -1) (begin (set! some-v n) n)
    0))

;(f 0) ;; 0 
;(f 1) ;; 1

(+ (f 0) (f 1)) ; 1
;(+ (f 1) (f 0)) ; 0
; left -> right : 0
; right -> left : 1