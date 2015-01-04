--main = print $ sum' identify 0 inc 10
main = print $ integral cube 0 1 0.01
--identify :: Int -> Int
identify a = a
--inc :: Int -> Int
inc a = a + 1
cube x = x * x * x

--sum' :: (Int -> Int) -> Float -> (Int -> Int) -> Int -> Int
sum' f a next b
  | a > b = 0
  | otherwise = f a + sum' f (next a) next b
sum'2 f a next b = 
  iter a 0
	where iter a result 
	        | a > b = 0
	        | otherwise = iter (next a) result + (f a)

integral f a b dx = 
  dx * sum'2 f (a + dx / 2.0) add_dx b
	where add_dx x = x + dx

--exercise 1.29 : simpsons rule ???
--simpsons :: (Int->Int) -> Int -> Int -> Int -> Float
--k :: Int -> Int -> Int 
--k b n
--  | n == 0 || n == b  = 1 
--  | rem n 2 == 0 = 2
--  | otherwise = 4
--simpsons f a b h =
--  (sum' (f (k b a)) a next b) / 3.0
--	where next n = if rem (n+1) 2 == 0 then n * ((b-a)/n) else n * h

