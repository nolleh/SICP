--exercise 2.33 

accumulate op initial = foldr op initial
map' p = accumulate (\x y-> p x:y) [] 
append seq1 seq2 = accumulate (\x y-> x:y) seq2 seq1
length' = accumulate (\_ y-> 1+y) 0

main = do
	print $ accumulate (+) 0 [1,2,3,4,5] --15
	print $ map' (*2) [1,2,3,4]
	print $ append [1,2,3,4] [5,6,7,8]
	print $ length' [1,2,3,4]