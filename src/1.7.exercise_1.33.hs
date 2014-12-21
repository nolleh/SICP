main = print $ accumulate' multiply 1 identity 1 inc 5

identity a = a
inc a = a + 1
plus a b = a + b
multiply a b = a * b

accumulate combiner null_value term a next b 
  | a > b = null_value
  | otherwise = combiner (term a) (accumulate combiner null_value term (next a) next b)

accumulate' combiner null_value term a next b = 
  iter a null_value
    where iter a result
  		    | a > b = null_value
  		    | otherwise = iter (next a) result `combiner` (term a)
