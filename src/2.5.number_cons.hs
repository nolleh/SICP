-- exercise 2.5
-- a 와 b 의 쌍을 2^a*3^b 로 표현한다면, cons / car / cdr ?
cons x y = 2^x*3^y
car z 
	| rem z 3 /= 0 = z
	| otherwise = car (div z 3)
cdr z 
	| rem z 2 /= 0 = z
	| otherwise = cdr (div z 2)

main = do 
	print $ car (cons 4 5)
	print $ cdr (cons 4 5)