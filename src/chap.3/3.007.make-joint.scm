(define (make-account balance pw)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (make-joint pw2)
    (lambda (pass msg)
      (if (eq? pass pw2) (dispatch msg)
          (error "Incorrect password2"))))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'make-joint) make-joint)
          (else (error "Unknown request -- MAKE-ACCOUNT") m)))
  (define (wrap-pw pass msg)
    (if (eq? pass pw) (dispatch msg)
        (error "Incorrect password")))
wrap-pw)

(define (make-joint acc match-pass new-pass)
  ((acc match-pass 'make-joint) new-pass))

;;; test code ;;;
(define peter-acc (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 30) ; 70
((paul-acc 'rosebud 'withdraw) 30) ; 30