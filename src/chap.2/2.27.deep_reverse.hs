import Data.Typeable
-- exercise 2.27. deep reverse
-- 바이너리 함수를 받아 인자를(x y) 넘겨 실행하는 함수 리턴 
cons x y = (\m-> m x y) 
-- 첫번째 인자를 리턴하는 함수를 z 의 인자로 하여 실행
car z = z (\p q-> p)  
cdr z = z (\p q-> q)
-- car $ cons x y = ((\p q-> p) x y) = x 

deep_reverse [] = []
deep_reverse pair
  | typeOf pair == typeOf 1 = pair
  | otherwise = cons (deep_reverse (cdr pair)) (deep_reverse (car pair)) 

main = do 
  --print $ deep_reverse $ (cons (cons 1 2) (cons 3 4))
  --print $ cons deep_reverse 2 deep_reverse 1
  -- cons 2 1