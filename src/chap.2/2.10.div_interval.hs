-- exercise 2.10
-- div-interval when zero

cons x y = (\m-> m x y)
car z = z (\p q-> p)
cdr z = z (\p q-> q)

make_interval x y = cons x y

-- 인자로 받은 함수에 두개의 인자를 받는 함수를 넘긴다.
lower_bound iv = iv (\p q-> min p q)
upper_bound iv = iv (\p q-> max p q)

mul_interval iv1 iv2 = let 
  p1 = (lower_bound iv1) * (lower_bound iv2)
  p2 = (lower_bound iv1) * (upper_bound iv2)
  p3 = (upper_bound iv1) * (lower_bound iv2) 
  p4 = (upper_bound iv1) * (upper_bound iv2) 
  in make_interval (minimum [p1, p2, p3, p4]) (maximum [p1, p2, p3, p4])

div_interval iv1 iv2
  | (upper_bound iv2 == 0 || lower_bound iv2 == 0) = undefined
  | otherwise = mul_interval iv1 (make_interval (1 / (upper_bound iv2)) (1 / (lower_bound iv2)))

display iv = 
  print (lower_bound iv, upper_bound iv)

main = do
  display $ div_interval (make_interval 6 15) (make_interval 2 3) -- (6/3, 15/2) 
  display $ div_interval (make_interval 0 1) (make_interval 0 3) -- 2.10.div_interval.hs: Prelude.undefined