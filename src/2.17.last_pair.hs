
last_pair [] = error "empty"
last_pair (x:[]) = x
last_pair xs = last_pair $ tail xs

main = do
  print $ last_pair [10, 20, 30, 40] -- 40