(define (make-account balance pw wrong-cnt-limit)
	(define (withdraw amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount))
				balance)
			"Insufficient funds"))
	(define (deposit amount)
		(set! balance (+ balance amount))
		balance)
	(define (dispatch m)
		(cond ((eq? m 'withdraw) withdraw)
			  ((eq? m 'deposit) deposit)
			  (else (error "Unknown request -- MAKE-ACCOUNT") m)))
	(define (wrap-pw pass msg)
		(if (eq? pass pw) (dispatch msg)
		(begin (set! wrong-cnt-limit (- wrong-cnt-limit 1))
			(if (<= wrong-cnt-limit 0) (call-the-cops)
			(error "Incorrect password .. remains:" wrong-cnt-limit)))))

	(define (call-the-cops)
		(display "sirens ringing!"))
wrap-pw)

;(define w (make-account 100))
;((w 'withdraw) 50)

(define acc (make-account 100 'secret-password 7))
((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
; 7 error> sirens ringing!