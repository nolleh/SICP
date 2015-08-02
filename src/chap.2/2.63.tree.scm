(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
    (list entry left right))

(define (tree->list-1 tree)
    (if (null? tree)
        '()
        (append (tree->list-1 (left-branch tree))
                (cons (entry tree)
                      (tree->list-1 (right-branch tree))))))
(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
            (cons (entry tree)
                (copy-to-list (right-branch tree)
                            result-list)))))
    (copy-to-list tree '()))
    
(tree->list-1 (make-tree 7 (make-tree 3 1 5) (make-tree 9 '() 11)))


; a. Do the two procedures produce the same result for every tree? 
; If not, how do the results differ? What
; lists do the two procedures produce for the trees in figure 2.16?

; b. Do the two procedures have the same order of growth 
; in the number of steps required to convert a
; balanced tree with n elements to a list? 
; If not, which one grows more slowly?