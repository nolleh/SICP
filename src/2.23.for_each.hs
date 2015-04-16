-- 2.23 for_each

-- is it possible ? 
for_each proc (x:[]) = (proc x)
for_each proc (x:xs) = 
  (proc x) (for_each proc xs)

main = do
  for_each (\x -> print x) [10, 20, 30, 40]