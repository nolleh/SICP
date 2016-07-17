(define (cycle?2 x)
  (let* ((one (cdr x))
        (two (cddr x)))
    (define (inner-cycle2 c1 c2)
      (cond ((or (not (pair? c1)) (< (length c2) 3)) #f)
            ((eq? c1 c2) #t)
            (else (inner-cycle2 (cdr c1) (cddr c2)))))
  (inner-cycle2 one two)))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
    x)

(cycle?2 '(a b))
(cycle?2 '(a b b a c))
(cycle?2 (make-cycle '(a b)))
; 어떻게 체크하지 넘어갔다는거를... ㅠ.ㅠ