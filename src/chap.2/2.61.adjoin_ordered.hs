adjoin_set item [] = [item]
adjoin_set item set@(x:xs)
  | item == x = set
  | item < x = item:set
  | otherwise = x:(adjoin_set item xs)
-- it traverse avg n/2
main = do
  print $ adjoin_set 1 [2,3,4]
  print $ adjoin_set 2 [1,3,4]