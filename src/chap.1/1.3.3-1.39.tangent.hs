-- | Main entry point to the application.
--1.39 tangent
module Main where
import Debug.Trace


tangent x k =
  cont_frac_iters (\n -> 2*n+1) func_d k
    where
      func_d :: Int -> Int
      func_d i
          | i == 0 = x
          | otherwise = x*x

cont_frac_iters :: (Int -> Int) -> (Int -> Int) -> Int -> Float
cont_frac_iters n d k = iter k 1.0
  where 
    iter :: Int -> Float -> Float
        iter i result
         | i == 0 = trace(show (n i)) result
         | otherwise = trace (show(n i)) 
          iter (i-1) ( fromIntegral (n i) / ( fromIntegral (d i) - result) )

main = do
  print $ tangent 3 5 
{-
    0.35786355
11
9
7
5
3
1
-}  