--Give a (n) implementation of union-set for sets represented as ordered lists.
adjoin_set item [] = [item]
adjoin_set item set@(x:xs)
  | item == x = set
  | item < x = item:set
  | otherwise = x:(adjoin_set item xs)

union_set [] set2 = set2
union_set (x:xs) set2 = union_set xs (adjoin_set x set2)
-- n/2 * n 
-- todo...
--union_set 

main = do 
  print $ union_set [1,2,3,4] [4,5,6,7] -- [1,2,3,4,5,6,7]
  print $ union_set [3,4,5] [1] -- [1,3,4,5]