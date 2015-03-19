
par1 r1 r2 = 
  div_interval (mul_interval r1 r2) (add_interval r1 r2)

par2 r1 r2 =
  let one = make_interval 1 1
  in div_interval one (add_interval (div_interval one r1) (div_interval one r2))

--- common {
make_interval x y = cons x y
cons x y = (\m-> m x y)
car z = z (\p q-> p)
cdr z = z (\p q-> q)

-- 인자로 받은 함수에 두개의 인자를 받는 함수를 넘긴다.
lower_bound iv = iv (\p q-> min p q)
upper_bound iv = iv (\p q-> max p q)

add_interval iv1 iv2 =
  make_interval (lower_bound iv1 + lower_bound iv2) (upper_bound iv1 + upper_bound iv2)

div_interval iv1 iv2
  | (upper_bound iv2 == 0 || lower_bound iv2 == 0) = undefined
  | otherwise = mul_interval iv1 (make_interval (1 / (upper_bound iv2)) (1 / (lower_bound iv2)))

mul_interval iv1 iv2 = let 
  p1 = (lower_bound iv1) * (lower_bound iv2)
  p2 = (lower_bound iv1) * (upper_bound iv2)
  p3 = (upper_bound iv1) * (lower_bound iv2) 
  p4 = (upper_bound iv1) * (upper_bound iv2) 
  in make_interval (minimum [p1, p2, p3, p4]) (maximum [p1, p2, p3, p4])

display iv = 
  print (lower_bound iv, upper_bound iv)  
---- } common

a = make_interval 30 10
b = make_interval 20 10

main = do
  display a
  display b
  display $ par1 a b
  display $ par2 a b

--(10.0,30.0)
--(10.0,20.0)
--(2.0,30.0)
--(5.0,11.999999999999998)
--[Finished in 0.4s]