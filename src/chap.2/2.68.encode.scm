
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

(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits)
           '()
           (let ((next-branch
                (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch)
                          (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))
(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
      ((= bit 1) (right-branch branch))
               (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))
                                    
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;(decode sample-message sample-tree)

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))
(define sample-decoded-message '(A D A B B C A))
         
(define (encode-symbol char tree)
    (define (iter-encode char tree bits)
        (if (leaf? tree)
            (if (eq? (symbol-leaf tree) char) bits
                (error "bad tree"))
            (let (next-branch (choose-branch char tree))
                (encode-symbol char tree))))
    (iter-encode char tree '0))
                
(define (choose-branch char tree)
    (if (element-of-set? char (symbols (left-branch tree)))
        (left-branch tree)
        (right-branch tree)))
(define (element-of-set? x set)
    (cond ((null? set) false)
          ((eq? x (car set)) true)
          (else (element-of-set? x (cdr set)))))
; (display (element-of-set? 'B (symbols (right-branch sample-tree))))
; (choose-branch 'A sample-tree)

; (display sample-decoded-message)
(encode sample-decoded-message sample-tree)
                