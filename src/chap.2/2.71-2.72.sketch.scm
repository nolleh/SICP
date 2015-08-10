; Exercise 2.71. Suppose we have a Huffman tree for an alphabet of n symbols, 
; and that the relative  frequenciesof the symbols are 1, 2, 4, ..., 2^(n-1). 
; Sketch the tree for n=5; for n=10. In such a tree (for
; general n) how many bits are required to encode the most frequent symbol? 
; => 1
; the least frequent symbol?
; => n-1


; ---------------------------- 

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


; Exercise 2.72. Consider the encoding procedure that you designed in exercise 2.68. 
; What is the order of growth in the number of steps needed to encode a symbol? 
; Be sure to include the number of steps needed
; to search the symbol list at each node encountered. 

; To answer this question in general is difficult. 

; Consider the special case where the relative frequencies 
; of the n symbols are as described in exercise 2.71, and give
; the order of growth (as a function of n) of the number of steps 
; needed to encode the most frequent and least
; frequent symbols in the alphabet.