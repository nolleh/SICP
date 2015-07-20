import Debug.Trace

--Give a (n) implementation of union-set for sets represented as ordered lists.
adjoin_set item [] = [item]
adjoin_set item set@(x:xs)
  | item == x = set
  | item < x = item:set
  | otherwise = x:(adjoin_set item xs)

--union_set [] set2 = set2
--union_set (x:xs) set2 = union_set xs (adjoin_set x set2)
-- n/2 * n 


-- improve ~~~~ 
union_set [] set2 = set2
union_set set1 [] = set1
union_set set1@(x:xs) set2@(y:ys) 
  | x > y = trace(".") y:(union_set set1 ys)
  | x < y = trace(".") x:(union_set xs set2)
  | otherwise = trace(".") x:(union_set xs ys)

main = do 
  print $ union_set [1,2,3,4] [4,5,6,7] -- [1,2,3,4,5,6,7] - 4times called
  print $ union_set [3,4,5] [1] -- [1,3,4,5] - 1 times called
  print $ union_set [0,1,2,3,4,5,6,7,8,9] [10,11,12,13,14,15,16,17,18,19]
  --[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19] - 10 times called
