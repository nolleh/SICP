--exercise 2.6
zero = (\f -> (\x->x))
add_1 n = (\f -> (\x-> f ((n f) x)))

one = (\f -> (\x-> f x))
two = (\f -> (\x-> f(f x)))

add n m = (\f -> (\x-> ((m f).(n f)) x ))

main = do 
	-- + 1 function 을 0 이라는 x 에 0번
	print $ zero (+ 1) 0  -- 0
	-- + 2 function 을 3 이라는 x 에 2번
	print $ two (+ 2) 3   -- 7
	-- + 1 function 을 0 이라는 x 에 1 번 한 후 + 1 을 한번 더
	print $ add_1 one (+ 1) 0  -- 2
	-- + 1 function 을 0 이라는 x 에 1 번 적용하고 다시 2 번 적용한다.
	print $ add one two (+ 1) 0  -- 3