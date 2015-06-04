--exercise 2.38 
main = do 
	print $ foldr (/) 1 [1,2,3] -- 0.5
	print $ foldl (/) 1 [1,2,3] -- 0.1666...
	print $ foldr (:) [] [1,2,3] -- 1,2,3 
	--print $ foldl (:) [] [1,2,3] -- undefined,