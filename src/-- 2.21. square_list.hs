-- 2.21. square_list
import Debug.Trace

square_list [] = error "empty"
square_list (x:[]) = [x * x]
square_list (x:xs) = [x * x] ++ (square_list xs)

square_list2 [] = error "empty"
--square_list2 (x:xs) = map (\x -> x*x) [x]++xs
square_list2 (x:xs) = map2 (\x -> x*x) ([x]++xs)

map2 f xs = iter f xs []
  where iter f xs out
          | null xs = out
          | otherwise = iter f (tail xs) (out++[f $ head xs]) 

main = do
  print $ square_list [1, 2, 3, 4]
  print $ square_list2 [1, 2, 3, 4]