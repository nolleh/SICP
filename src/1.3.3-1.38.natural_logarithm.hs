--1.38 natural logarithms
import Debug.Trace
natural_logarithm k = 
  cont_frac_iters (\n -> 1) func_d k
    where 
    	func_d :: Int -> Int
    	func_d i
          | rem (i - 1) 3 == 0 = ((i - 1) `div` 3 + 1) * 2
          | otherwise = 1

cont_frac_iters :: (Int -> Int) -> (Int -> Int) -> Int -> Float
cont_frac_iters n d k = iter k 1.0
  where 
  	iter :: Int -> Float -> Float
        iter i result
         | i == 0 = trace (show(d i)) result
         | otherwise = trace (show(d i)) 
         	iter (i-1) ( fromIntegral (n i) / ( fromIntegral (d i) + result) )

main = do
  print $ natural_logarithm 10

{-|
  8 1 1 6 1 1 4 1 1 2 1
  0.39221117 
-}