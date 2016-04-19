(define (estimate-integral p x1 x2 y1 y2 trials)
  (* (* (- x1 x2) (- y1 y2)) 
    (monte-carlo trials (p x1 x2 y1 y2))))

; rectangle 은 주어지고, p(x,y) 에 rectangle 을 넣어 
; 다음에 x,y 인자를 받아 이 영역 안에 있으면 true 를 뱉는 함수 생성

; (x − 5)^2 + (y − 7)^2 ≤ 3^2
(define (is-in-range x1 x2 y1 y2)
  (let* ((r (- x1 x2))
        (cx (+ x2 (/ r 2)))
        (cy (+ y2 (/ r 2)))
        (x (random-in-range x2 x1))
        (y (random-in-range y2 y1)))
    (lambda ()
      (<= (expt (- x cx) 2) (expt (- y cy) 2)
        (expt r 2)))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
            (/ trials-passed trials))
          ((experiment)
            (iter (- trials-remaining 1)
                  (+ trials-passed 1)))
            (else
              (iter (- trials-remaining 1)
                    trials-passed))))
  (iter trials 0))

(estimate-integral is-in-range 5 0 5 0 10)