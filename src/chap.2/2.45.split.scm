(define right-split (split beside below))
(define up-split (split below beside))

(define split p1 p2 painter n)
(if (= n 0) 
	painter
	(let ((smaller (right-split painter (- n 1)))))
	(p1 painter (p2 smaller smaller)))