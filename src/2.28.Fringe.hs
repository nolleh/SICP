-- exercise 2.28. Fringe
import Debug.Trace
fringe (x:[]) = [x]
fringe (x:xs) = trace ("x:"++show x++",xs"++show xs) (fringe [x] ++ fringe xs )

-- scheme 에서는 pair 로 단일인지 쌍인지 같은 함수에서 구분했던거보면 
-- 그냥 이렇게 풀면 되는거 같다... 

main = do
  print $ fringe [[1,2], [3,4]]
  --print $ fringe [1,2,3,4]
  --print $ test [1,2,3,4]