; we know tree->list ( 2n )
; list->tree ( n )
;

(define (union_tree tree1 tree2) ; => 6N complexity => N
	(let (list1 (tree->list-2 tree1)) ; (2N complexity)
		 (list2 (tree->list-2 tree2)) ; (2N complexity)
		 (list->tree (union_set list1 list2)))) ; (N + N complexity)

(define (intersection_tree tree1 tree2) ; => 6N complexity => N
	(let (list1 (tree->list-2 tree1)) ; (2N complexity)
		 (list2 (tree->list-2 tree2)) ; (2N complexity)
		 (list->tree (intersection_set list1 list2)))) ; (N + N complexitity)