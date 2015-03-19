reverse' xs =
  iter xs []
  where 
    iter (x:[]) out = [x] ++ out
    iter (x:xs) out = iter xs ([x] ++ out)

main = do
  print $ reverse' [10, 20, 30, 40] -- [40,30,20,10]
