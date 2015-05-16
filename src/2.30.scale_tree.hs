{-# LANGUAGE MultiParamTypeClasses, OverlappingInstances, FlexibleInstances #-}
-- exercise 2.30.
-- square_tree

-- without higher order functions.
class ScaleTree i o where
  scale_tree :: Int -> [i] -> [o]

instance ScaleTree i o where
-- leaf
  scale_tree factor tree = factor * tree

instance ScaleTree i o => ScaleTree [i] [o] where
-- when tree is list. just propagate to it's leaf
  scale_tree factor = map (scale_tree factor)


main = do
  --print $ (scale_tree [1, [2, [3, 4], 5], [6, 7]] 10 :: [[[Int]]])
  print $ (scale_tree 2 [1,2,3] :: [Int])


--scheme
--define squere_tree tree
--  if (pair? tree) (cons (square_tree (car tree)) 
--                        (square_tree (cdr tree)))
--  (car tree) * (car tree)

--define squere_tree tree
--  map (lambda sub_tree = if (pair? sub_tree) (square_tree sub_tree)
--                            (car sub_tree) * (car sub_tree) 
--      tree)