; a. Add some segments to the primitive wave painter
(define (make-segment v1 v2)
  (cons v1 v2))

(define wave-segments
 (list
  (make-segment
    (make-vect 0.0 0.5)
    (make-vect 1.0 0.5)) ; horizontal line in center
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))))


; b. Change the pattern constructed by corner-split 
; (for example, by using only one copy of the upsplit
; and right-split images instead of two).

(define (corner-split painter n)
  (if (= n 0)
    painter
  (let ((up (up-split painter (- n 1)))
        (right (right-split painter (- n 1))))
    (let ((top-left (beside up up))
          (bottom-right (below right right))
          (corner (corner-split painter (- n 1))))
      (beside (below painter top-left)
              (below bottom-right corner))))))



; c. Modify the version of square-limit that uses square-of-four so as to assemble the corners in a
; different pattern. (For example, you might make the big Mr. Rogers look outward from each corner of the
; square.)

;모서리, 바깥쪽보도록.
(define (square-limit painter n)
  (let (combine4 (square-of-four identity flip-horiz
                                 flip-vert rotate180)))
  (combine4 (corner-split painter n)))