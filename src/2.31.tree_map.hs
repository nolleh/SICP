--exercise 2.31
--scheme

--define (square_tree tree) (tree_map square tree)
    define tree_map proc tree
      if (pair? tree) cons (tree_map (car tree)
                            tree_map (cdr tree))
                      (proc tree)