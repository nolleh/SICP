-- infinite fraction

-- recursive
cont_frac_recurs :: (Int -> Float) -> (Int -> Float) -> Int -> Float
cont_frac_recurs n d k = iter 0
  where iter :: Int -> Float
  	iter i 
         | i == k = (n i) / (d i)
         | otherwise = (n i) / ( (d i) + (iter (i+1)) )

cont_frac_iters :: (Int -> Float) -> (Int -> Float) -> Int -> Float
cont_frac_iters n d k = iter k 1
  where 
  	iter :: Int -> Float -> Float
        iter i result
         | i == 0 = result
         | otherwise = iter (i-1) ( (n i) / ((d i) + result) )

main = do
  print $ cont_frac_recurs (\i->1.0) (\i->1.0) 10 -- 0.6180556
  print $ cont_frac_iters (\i->1.0) (\i->1.0) 10 -- 0.6180556