--exercise 2.40 
enumerate_interval low high = [x | x<-[low..high] ]
unique_pairs n = 
	foldr (++) [] 
		map (\i-> map (\j-> [i,j]) (enumerate_interval 1 i-1))
		enumerate_interval 1 n

main = do
	--print $ enumerate_interval 1 7
	print $ unique_pairs 3