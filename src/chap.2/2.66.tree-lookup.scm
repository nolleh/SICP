; lookup

(define (lookup given-key tree-of-records)
	(cond ((null? tree-of-records) false)
		  ((equal? given-key (key (entry tree-of-records)))
		  	  (entry tree-of-records))
		  (else (let (left (lookup given-key (left-branch tree-of-records)))
		  		(if (equal? false left)
		  			(lookup given-key (right-branch tree-of-records))
		  			(left))))