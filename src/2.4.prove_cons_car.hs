--exercise 2.4 
-- 인자로 받은 함수를 x 와 y 를 인자로하여 실행하는 함수 리턴
--cons :: Int -> Int -> (Int -> Int -> Int) -> Int
cons x y = (\m-> m x y) 
-- z 에 p 와 q 를 받아 p 를 리턴하는 함수를 인자로 하여 z 함수 실행
--car :: ((Int -> Int -> Int) -> Int) -> Int
car z = z (\p q-> p) 
cdr z = z (\p q-> q)

main = do 
	print $ car (cons 2 3)
	print $ cdr (cons 2 3)