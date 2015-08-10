; 2.70.lyrics.scm
; A 2 NA 16
; BOOM 1 SHA 3
; GET 2 YIP 9
; JOB 2 WAH 1
; order-set : ((BOOM 1) (WAH 1) (A 2) (GET 2) (JOB 2) (SHA 3) (YIP 9) (NA 16))
; merge : (({BOOM WAH} 2) (A 2) (GET 2) (JOB 2) (SHA 3) (YIP 9) (NA 16))
; merge : (({BOOM WAH A} 4) (GET 2) (JOB 2) (SHA 3) (YIP 9) (NA 16))
; merge : (({BOOM WAH A} 4) (({GET JOB} 4) (SHA 3) (YIP 9) (NA 16))
; merge : (({BOOM WAH A} 4) (({GET JOB SHA} 7) (YIP 9) (NA 16)))
; merge : (({BOOM WAH A GET JOB SHA} 11) (YIP 9) (NA 16))
; merge : (({BOOM WAH A GET JOB SHA YIP} 20) (NA 16))
; final merge : ({BOOM WAH A GET JOB SHA YIP NA} 36)

; (generage-huffman-tree sample)


; ((((((leaf BOOM 1) (leaf WAH 1) (BOOM WAH) 2) (leaf A 2) (BOOM WAH A) 4) 
; (((leaf Get 2) (leaf Job 2) (Get Job) 4) (leaf SHA 3) (Get Job SHA) 7) 
; (BOOM WAH A Get Job SHA) 11) (leaf YIP 9) (BOOM WAH A Get Job SHA YIP) 20) (leaf NA 16) 
; (BOOM WAH A Get Job SHA YIP NA) 36) 

(define song-tree 
  (make-code-tree
    (make-code-tree
        ; ({BOOM WAH A GET JOB SHA} 11)
        (make-code-tree 
            ; {BOOM, WAH, A}
            (make-code-tree (make-code-tree (make-leaf 'BOOM 1) (make-leaf 'WAH 1)) (make-leaf 'A 2))
            ; ({GET, JOB, SHA} 7)
            (make-code-tree (make-code-tree (make-leaf 'Get 2) (make-leaf 'Job 2)) (make-leaf 'SHA 3)))

        (make-leaf 'YIP 9)) (make-leaf 'NA 16)))

; duplicate
(define (make-leaf symbol weight)
    (list 'leaf symbol weight))
(define (leaf? object)
    (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree)
       (list (symbol-leaf tree))
       (caddr tree)))
(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))


(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))
(define sample-decoded-message '(A D A B B C A))
         
(define (encode-symbol char tree)
    (if (leaf? tree)
        (if (eq? (symbol-leaf tree) char) '()
            (error "bad tree"))
            (if (element-of-set? char (symbols (left-branch tree)))
                (cons 0 (encode-symbol char (left-branch tree)))
                (cons 1 (encode-symbol char (right-branch tree))))))
                
(define (choose-branch char tree)
    (if (element-of-set? char (symbols (left-branch tree)))
        (left-branch tree)
        (right-branch tree)))

(define (element-of-set? x set)
    (cond ((null? set) #f)
          ((eq? x (car set)) #t)
          (else (element-of-set? x (cdr set)))))



(define first '(Get a job))
(define second '(Sha na na na na na na na na))
(define third '(Wah yip yip yip yip yip yip yip yip yip))
(define forth '(Sha boom))

; How many bits are required for the encoding? 
(display (encode first song-tree)) ; (0 0 1 0 0 0 0 0 1 0 0 1 0 1) - 14
(display (encode second song-tree)) ; (0 0 1 1 1 1 1 1 1 1 1 1) - 12
(display (encode third song-tree)) ; (0 0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1) - 23
(display (encode forth song-tree)) ; (0 0 1 1 0 0 0 0 0) - 9

; What is the smallest number of bits that would be needed to
; encode this song if we used a fixed-length code for the eight-symbol alphabet?

; forth. (8 * 2)
