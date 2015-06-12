--2.36 accumulate_n
accumulate_n op initial [] = []
accumulate_n op initial seqs@(x:xs) =
  case x of
      [] -> [] 
      otherwise -> (foldr op initial (map head seqs)) : (accumulate_n op initial (map tail seqs))

main = do
  print $ accumulate_n (+) 0 [[1,2,3],[4,5,6],[7,8,9],[10,11,12]] --[22,26,30]