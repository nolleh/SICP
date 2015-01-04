main = do 
  print $ filtered_accumulate prime plus 0 square 1 inc 4
  print $ filtered_accumulate (relative_prime 5) multiply 1 identity 1 inc 5
identity a = a
square a = a * a
inc a = a + 1
plus a b = a + b
multiply a b = a * b
prime a =
  a == smallest_divisor a
    where 
      smallest_divisor a = 
    	find_divisor a 2
		  where find_divisor n test_divisor
		          | square test_divisor > n = n
		          | rem n test_divisor == 0 = test_divisor
		          | otherwise = find_divisor n (test_divisor + 1)
relative_prime n i
  | 0 < i && i < n && gcd i n == 1 = True
  | otherwise = False

filtered_accumulate predicate combiner null_value term a next b = 
  iter a null_value
    where iter a result
  		    | a > b = result
  		    | predicate a == True = iter (next a) (combiner result (term a))
  		    | otherwise = iter (next a) result

filtered_accumulate' bi_predicate combiner null_value term a next b = 
  iter a null_value
    where iter a result
  		    | a > b = result
  		    | bi_predicate a == True = iter (next a) (combiner result (term a))
  		    | otherwise = iter (next a) result
