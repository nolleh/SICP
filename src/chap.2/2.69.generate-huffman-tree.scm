(define (generate-huffman-tree pairs)
	(successive-merge (make-leaf-set pairs)))

; 페어리스트를 인자로 받아 ordered leaf set 으로 바꿈
(define (make-leaf-set pairs)
	(if (null? pairs)
		'()
		(let ((pair (car pairs)))
			(adjoin-set (make-leaf (car pair)
								   (cadr pair))
						(make-leaf-set (cdr pairs))))))

(define (element-of-set? x set)
    (cond ((null? set) #f)
          ((eq? x (car set)) #t)
          (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
	(if (element-of-set? x set)
		set
		(cons x set)))

;(define sample-tree
;    (make-code-tree (make-leaf 'A 4)
;                    (make-code-tree
;                    (make-leaf 'B 2)
;                    (make-code-tree (make-leaf 'D 1)
;                                    (make-leaf 'C 1)))))


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

;((leaf h 1) (leaf g 1) (leaf f 1) (leaf e 1) (leaf d 1) (leaf c 1) (leaf b 3) (leaf a 8))
(define sample-leaf-set (make-leaf-set (list (list 'H 1) (list 'G 1) (list 'F 1) (list 'E 1) 
							  (list 'D 1) (list 'C 1) (list 'B 3) (list 'A 8))))

(display sample-leaf-set)

; (((leaf h 1) (leaf g 1) (h g) 2) (leaf f 1) (leaf e 1) (leaf d 1) (leaf c 1) (leaf b 3) (leaf a 8))
(display (cons (make-code-tree (car sample-leaf-set) (cadr sample-leaf-set)) (cddr sample-leaf-set)))

;(define (successive-merge leaf-set)
;	(cond ((null? leaf-set) '())
;		  (else (successive-merge (cons (make-code-tree (car leaf-set) (cadr leaf-set)) 
;		  		 (cddr leaf-set))))))

;(((leaf h 1) (leaf g 1) (h g) 2) ((leaf f 1) (leaf e 1) (f e) 2) ((leaf d 1) (leaf c 1) (d c) 2) ((leaf b 3) (leaf a 8) (b a) 11))
;(display (successive-merge sample-leaf-set))
