-- duplicates
-- same complexity with non-duplicated ones.
element_of_set item [] = False
element_of_set item set@(x:xs)
  | item == x = True
  | otherwise = element_of_set item xs


-- always put. it's complexity is C (before - n)
adjoin_set x set = x:set

-- n. before - n^2
union_set [] set2 = set2
union_set (x:xs) set2 = union_set xs (adjoin_set x set2)


-- n^2.
intersection_set [] set2 = []
intersection_set (x:xs) set2
  | element_of_set x set2 = x:intersection_set xs set2
  | otherwise = intersection_set xs set2


main = do
  print $ intersection_set [1,2,3] [2,3,4] --[2,3]