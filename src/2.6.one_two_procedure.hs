--exercise 2.6
zero = (\f -> (\x->x))
add_1 n = (\f -> (\x-> f ((n f) x)))

main = do 
	print $ add_1 zero