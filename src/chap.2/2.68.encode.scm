
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
; (display (element-of-set? 'B (symbols (right-branch sample-tree))))
; (display (choose-branch 'A sample-tree))
; (display (encode-symbol 'A (list 'leaf 'a 4)))
(display (encode sample-decoded-message sample-tree)) ; (0 1 1 0 0 1 0 1 0 1 1 1 0)