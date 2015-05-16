--Exercise 1.36
--modify fixed_point show it's process using newline / display
--x->log(1000)/log(x)
--compute # of steps. (with average damp/without avg damp)
import Debug.Trace

average :: (Fractional a) => a -> a -> a
average x y = (x+y) / 2

fixed_point f first_guess = try first_guess 0
  where 
  	close_enough v1 v2 = abs (v1-v2) < tolerance
  	tolerance = 0.00001
  	try guess count
  	  | close_enough guess next = trace ("# of process:"++ show count) next
  	  | otherwise = try next (count+1)
  	  where next = f guess

main = do
  print $ fixed_point (\x -> (log 1000) / (log x)) 2 
  -- without # of process:33
  --4.555532270803653
  print $ fixed_point (\x -> (average x ((log 1000) / (log x)))) 2
  -- with average # of process:8
  --4.555537551999825