(define (flip_horiz painter)
	(transform_painter painter
					   (make_vect (1,1))
					   (make_vect (0,0))
					   (make_vect (0,1))))

; 시계 반대 방향 180, 270

(define (rotate-180 painter)
	(transform_painter painter
					   (make_vect (1,1))
					   (make_vect (0,1))
					   (make_vect (1,0))))

(define (rotate-270 painter
	(transform_painter painter
					   (make_vect (0,1))
					   (make_vect (0,0))
					   (make_vect (1,1))))
