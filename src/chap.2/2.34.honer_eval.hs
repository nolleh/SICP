--exercise 2.34
accumulate = foldr

horner_eval x = 
	accumulate (\this_coeff higher_terms-> this_coeff + higher_terms*x) 0

main = do
	print $ horner_eval 2 [1,3,0,5,0,1] -- 79