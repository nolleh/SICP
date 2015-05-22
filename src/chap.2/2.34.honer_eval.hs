--exercise 2.34
accumulate = foldr

horner_eval x = 
	accumulate (\this_coeff higher_terms-> (x*this_coeff + higher_terms)*x) 0

main = do
	--print $ accumulate (+) 0 [1,2,3,4,5] --15
	print $ horner_eval 2 [1,3,0,5,0,1] -- 316