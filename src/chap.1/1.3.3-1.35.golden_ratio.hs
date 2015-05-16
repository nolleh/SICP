--Exercise 1.35. 
--Show that the golden ratio (section 1.2.2) is a fixed point of 
--the transformation x 1 + 1/x, 
--and use this fact to compute by means of 
--the fixed-point procedure.

fixed_point f first_guess = try first_guess
  where 
  	close_enough v1 v2 = abs (v1-v2) < tolerance
  	tolerance = 0.00001
  	try guess
  	  | close_enough guess next = next
  	  | otherwise = try next
  	  where next = f guess

main = do
  print $ fixed_point (\x -> 1+1/x) 1
--1.6180327868852458
--[Finished in 0.4s]