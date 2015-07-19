-- Exercise 2.59. Implement the union-set operation 
-- for the unordered-list representation of sets.
element_of_set item [] = False
element_of_set item set@(x:xs)
  | item == x = True
  | otherwise = element_of_set item xs

adjoin_set x set
  | element_of_set x set = set
  | otherwise = x:set

union_set set1@(x:_) set2
  | (null set1 || null set2) = set2
  | otherwise = adjoin_set x set2 

main = do
  --print $ element_of_set 3 []
  --print $ adjoin_set 3 []
  print $ union_set [1,2,3,4] [4,5,6,7] -- [1,4,5,6,7]
  print $ union_set [3,4,5] [1] -- [3,1]