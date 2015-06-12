--exercise 2.41
--i,j,k <= n SUM(i,j,k) == S
--

unique_pairs n = 
	foldr (++) [] 
		(concatMap (\i-> map (\j-> 
			map (\k-> [i,j,k]) [1..(j-1)]) [1..(i-1)])
		[1..n])

tripple_sum n s = 
	(filter (\(x:y:z:[])-> (x+y+z)==s) (unique_pairs n))

main = do
	print $ tripple_sum 10 8 --[[4,3,1],[5,2,1]]