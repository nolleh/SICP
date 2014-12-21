--main = print $ product' term 1 inc 4
--2/3 * 4/3 * 4/5 * 6/5 * 6/7 * 8/7 ..
main = print $ product' term 1 inc 3
term :: Int -> Double
term n = fromIntegral numer / fromIntegral denom  
  where 
  	numer
	  | rem n 2 == 0 = n + 2
	  | otherwise = n + 1
	denom 
	  | rem n 2 == 0 = n + 1
	  | otherwise = n + 2

inc a = a + 1

product' f a next b
  | a > b = 1
  | otherwise = f a * product' f (next a) next b
product'2 f a next b = 
  iter a 1
	where iter a result 
	        | a > b = 1
	        | otherwise = iter (next a) result * (f a)