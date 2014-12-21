--main = print $ filtered_accumulate' prime plus 0 square 1 inc 5
main = print $ filtered_accumulate'' relative_prime multiply 1 identity 1 inc 5
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
relative_prime i n
  | 0 < i && i < n && gcd i n == 1 = True
  | otherwise = False

filtered_accumulate' predicate combiner null_value term a next b = 
  iter a null_value
    where iter a result
  		    | a > b = null_value
  		    | predicate a == True = iter (next a) result `combiner` (term a)
  		    | otherwise = iter (next a) result

filtered_accumulate'' bi_predicate combiner null_value term a next b = 
  iter a null_value
    where iter a result
  		    | a > b = null_value
  		    | bi_predicate a b == True = iter (next a) result `combiner` (term a)
  		    | otherwise = iter (next a) result
