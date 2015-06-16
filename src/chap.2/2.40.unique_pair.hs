--exercise 2.40 
--enumerate low high = [x | x <- [low..high] ]
unique_pairs n = 
	foldr (++) [] 
		(map (\i-> map (\j-> [i,j]) [1..(i-1)]) [1..n])

-- [[1,2],[1,3]]
--prime_sum_pairs :: Int -> [Int]
prime_sum_pairs n = --[[2,1],[3,2]]
	map (\(x:y:[])-> x:y:[(x+y)]) 
	(filter (\(x:y:[])-> is_prime (x+y)) (unique_pairs n))

--isprime :: (Num a)=>a -> [a]
-- fromIntegral : to make floating number. (sqrt)
is_prime n = all (\x-> (mod n x)/=0) [2..(floor (sqrt $ fromIntegral n))]
--isPrime :: Integer->Bool
--isPrime x = ([] == [y | y<-[2..floor (sqrt (fromIntegral x))], (rem x y) == 0])
--test (x:xs) = x+xs

main = do
	print $ unique_pairs 3 -- [[2,1],[3,1],[3,2]]
	print $ prime_sum_pairs 3 -- [[2,1,3],[3,2,5]]
	--print $ test [2,3]